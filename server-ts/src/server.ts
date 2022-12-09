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
  console.log(`Were initialized...`)
  if (hasConfigurationCapability) {
    console.log(`...And has config capabilities`)
    // Register for all configuration changes.
    connection.client.register(
      DidChangeConfigurationNotification.type,
      undefined
    )
  } else {
    console.log(`...Without config capabilities`)
  }

  if (hasWorkspaceFolderCapability) {
    console.log(`...And with Workspace Folder Capabilities`)
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log('Workspace folder change event received.')
    })
  } else {
    console.log(`...Without Workspace Folder Capabilities`)
  }
})

// The example settings
interface ExampleSettings {
  maxNumberOfProblems: number
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 }
let globalSettings: ExampleSettings = defaultSettings

// Cache the settings of all open documents
const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map()

connection.onDidChangeConfiguration((change) => {
  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear()
  } else {
    globalSettings = <ExampleSettings>(
      (change.settings.languageServerExample || defaultSettings)
    )
  }

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument)
})

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings)
  }

  let result = documentSettings.get(resource)

  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: 'pikeLangServer',
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
  // const settings = await getDocumentSettings(textDocument.uri)

  // // The validator creates diagnostics for all uppercase words length 2 and more
  // const text = textDocument.getText()
  // const pattern = /\b[A-Z]{2,}\b/g
  // let m: RegExpExecArray | null

  // let problems = 0

  console.log(`CWD:`, process.cwd())

  const path = textDocument.uri.replace('file://', '')

  const spawnResult = await new Promise<string | Error>((resolve) => {
    const args = [
      '-M',
      join(__dirname, '..', '..', 'pike-modules'),
      join(__dirname, '..', 'bin', 'compiler.pike'),
      path,
    ]

    const spawner = spawn('pike', args)

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
