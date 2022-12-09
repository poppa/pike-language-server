import { spawn } from 'child_process'
import { join } from 'path'
import { TextDocument } from 'vscode-languageserver-textdocument'
import {
  CompletionItem,
  createConnection,
  Diagnostic,
  DiagnosticSeverity,
  DidChangeConfigurationNotification,
  InitializeResult,
  ProposedFeatures,
  TextDocumentPositionParams,
  TextDocuments,
  TextDocumentSyncKind,
} from 'vscode-languageserver/node'

const connection = createConnection(ProposedFeatures.all)
const documents = new TextDocuments(TextDocument)

let hasConfigurationCapability = false
let hasWorkspaceFolderCapability = false
let hasDiagnosticRelatedInformationCapability = false

connection.onInitialize((params) => {
  const capabilities = params.capabilities

  // Does the client support the `workspace/configuration` request?
  // If not, we fall back using global settings.
  hasConfigurationCapability = !!capabilities.workspace?.configuration
  hasWorkspaceFolderCapability = !!capabilities.workspace?.workspaceFolders
  hasDiagnosticRelatedInformationCapability =
    !!capabilities.textDocument?.publishDiagnostics?.relatedInformation

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Tell the client that this server supports code completion.
      completionProvider: {
        resolveProvider: true,
      },
    },
  }

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    }
  }

  return result
})

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    // Register for all configuration changes.
    connection.client.register(
      DidChangeConfigurationNotification.type,
      undefined
    )
  }

  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      // console.log('Workspace folder change event received:', _event)
      // connection.console.log('Workspace folder change event in Server')
    })
  }
})

// FIXME: Move this to a shareable package
interface PikeSettings {
  maxNumberOfProblems: number
  trace: {
    server: boolean
  }
  compiler: {
    path: string
  }
  runtime: {
    includePaths?: string[]
    modulePaths?: string[]
  }
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: PikeSettings = {
  maxNumberOfProblems: 100,
  trace: { server: false },
  compiler: { path: 'pike' },
  runtime: {},
}
let globalSettings: PikeSettings = defaultSettings
// Cache the settings of all open documents
const documentSettings: Map<string, Thenable<PikeSettings>> = new Map()

connection.onDidChangeConfiguration((change) => {
  console.log(`Got Config Change: %o`, change)

  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear()
  } else {
    globalSettings = (change.settings.pikeLanguageServer ||
      defaultSettings) as PikeSettings
  }

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument)
})

function getDocumentSettings(resource: string): Thenable<PikeSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings)
  }

  let result = documentSettings.get(resource)

  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: 'pikeLanguageServer',
    })

    documentSettings.set(resource, result)
  }

  return result
}

// Only keep settings for open documents
documents.onDidClose((e) => {
  documentSettings.delete(e.document.uri)
})

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change) => {
  // validateTextDocument(change.document)
})

documents.onDidSave((e) => {
  console.log(`Saved...`, e)
  validateTextDocument(e.document)
})

interface PikeError {
  line: number
  file: string
  msg: string
}

interface PikeResult {
  filename: string
  errors: PikeError[]
  warnings: PikeError[]
}

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  console.log(`Validate Text Document ... %o`, textDocument)

  // // In this simple example we get the settings for every validate run.
  const settings = await getDocumentSettings(textDocument.uri)

  const path = textDocument.uri.replace('file://', '')

  const spawnResult = await new Promise<string | Error>((resolve) => {
    const args = [
      '-M',
      join(__dirname, '..', '..', 'pike-modules'),
      join(__dirname, '..', 'bin', 'compiler.pike'),
      path,
    ]

    const pikeEnv = {
      ...process.env,
    }

    if (settings.runtime.includePaths) {
      let incPaths: string[] = []

      if (process.env.PIKE_INCLUDE_PATH) {
        incPaths = [process.env.PIKE_INCLUDE_PATH]
      }

      pikeEnv.PIKE_INCLUDE_PATH = [
        ...incPaths,
        ...settings.runtime.includePaths,
      ].join(':')
    }

    if (settings.runtime.modulePaths) {
      let modPaths: string[] = []

      if (process.env.PIKE_MODULE_PATH) {
        modPaths = [process.env.PIKE_MODULE_PATH]

        if (modPaths[0].endsWith(':')) {
          modPaths[0] = modPaths[0].substring(0, modPaths[0].length - 1)
        }
      }

      pikeEnv.PIKE_MODULE_PATH = [
        ...modPaths,
        ...settings.runtime.modulePaths,
      ].join(':')
    }

    const spawner = spawn('pike', args, { env: pikeEnv })

    const cid = setTimeout(() => {
      spawner.kill(9)
    }, 2000)

    const outBuffer: Buffer[] = []
    const errBuffer: Buffer[] = []

    spawner.stdout.on('data', (chunk) => {
      outBuffer.push(chunk)
    })

    spawner.stderr.on('data', (chunk) => {
      errBuffer.push(chunk)
    })

    spawner
      .on('error', (err) => {
        console.log(`Got Error:`, err)
        clearTimeout(cid)
        resolve(err)
      })
      .on('exit', (code) => {
        clearTimeout(cid)

        if (code === 0) {
          if (errBuffer.length) {
            console.log(
              `Spawn pike wrote to stderr: %s`,
              errBuffer.map((b) => b.toString('utf-8')).join('')
            )
          }

          resolve(outBuffer.map((b) => b.toString('utf-8')).join(''))
        } else {
          resolve(new Error(errBuffer.map((b) => b.toString('utf-8')).join('')))
        }
      })
  })

  if (spawnResult instanceof Error) {
    console.error(`Failed:`, spawnResult)
    return
  }

  const messages: PikeResult[] = JSON.parse(spawnResult || '[]')

  const toDiagnostic = (
    message: PikeError,
    severity: DiagnosticSeverity
  ): Diagnostic => {
    return {
      message: message.msg,
      range: {
        start: {
          line: message.line,
          character: 0,
        },
        end: {
          line: message.line,
          character: 100,
        },
      },
      severity,
    }
  }

  const diagnostics: Diagnostic[] = []

  for (const message of messages) {
    if (message.errors.length) {
      diagnostics.push(
        ...message.errors.map((pe) =>
          toDiagnostic(pe, DiagnosticSeverity.Error)
        )
      )
    }

    if (message.warnings.length) {
      diagnostics.push(
        ...message.warnings.map((pw) =>
          toDiagnostic(pw, DiagnosticSeverity.Warning)
        )
      )
    }
  }

  console.log(`Diagnostics:`, diagnostics)

  // Send the computed diagnostics to VSCode.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics })
}

connection.onDidChangeWatchedFiles((_change) => {
  // Monitored files have change in VSCode
  connection.console.log('We received an file change event')
})

// This handler provides the initial list of the completion items.
connection.onCompletion(
  (_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
    console.log(`Got onCompletions @ %o`, _textDocumentPosition)
    return []
    // The pass parameter contains the position of the text document in
    // which code complete got requested. For the example we ignore this
    // info and always provide the same completion items.
    // return [
    //   {
    //     label: 'TypeScript',
    //     kind: CompletionItemKind.Text,
    //     data: 1,
    //   },
    //   {
    //     label: 'JavaScript',
    //     kind: CompletionItemKind.Text,
    //     data: 2,
    //   },
    // ]
  }
)

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
  console.log(`Got On CompletionResolve`)
  // if (item.data === 1) {
  //   item.detail = 'TypeScript details'
  //   item.documentation = 'TypeScript documentation'
  // } else if (item.data === 2) {
  //   item.detail = 'JavaScript details'
  //   item.documentation = 'JavaScript documentation'
  // }
  return item
})

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection)

// Listen on the connection
connection.listen()
