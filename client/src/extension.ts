import * as path from 'path'
import {
  workspace,
  ExtensionContext,
  TextDocument,
  WorkspaceFolder,
  Uri,
} from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node'
import { PikeClientConfig } from './config'

let defaultClient: LanguageClient
let clients = new Map<string, LanguageClient>()
let _sortedWorkspaceFolders: string[] | undefined

function isPikeDocument(document: TextDocument): boolean {
  return document.languageId === 'pike'
}

export function activate(context: ExtensionContext) {
  console.log(`Activating client: %O`, context)

  function didOpenTextDocument(document: TextDocument): void {
    console.log(`Open Document:`, document.uri.scheme)

    if (!isPikeDocument(document)) {
      console.log(
        `Not a pike document: langid: %O, uri: %O`,
        document.languageId,
        document.uri.scheme
      )
      return
    }

    let uri = document.uri
    let folder = workspace.getWorkspaceFolder(uri)
    const config = new PikeClientConfig()

    // Untitled files go to a default client.
    if (!folder || uri.scheme === 'untitled') {
      console.log(`Untitled or no Folder`)
      if (!defaultClient) {
        console.log(
          `create default client: Workspace conf: %O`,
          workspace.getConfiguration().get('[pike]')
        )
        defaultClient = new PikeClient(context, config)
        defaultClient.start()
      } else {
        console.log(`Default client already exist`)
      }

      return
    }

    // If we have nested workspace folders we only start a server on the outer
    // most workspace folder.
    folder = getOuterMostWorkspaceFolder(folder)

    if (!clients.has(folder.uri.toString())) {
      console.log(`Create client for workspace`)
      let client = new PikeClient(context, config)
      client.start()
      clients.set(folder.uri.toString(), client)
    } else {
      console.log(`Workspace alread has client`)
    }
  }

  workspace.onDidOpenTextDocument(didOpenTextDocument)
  workspace.textDocuments.forEach(didOpenTextDocument)
  workspace.onDidChangeWorkspaceFolders((event) => {
    for (let folder of event.removed) {
      let client = clients.get(folder.uri.toString())
      if (client) {
        console.log(`Remove client: %O\n`, client)
        clients.delete(folder.uri.toString())
        client.stop()
      }
    }
  })
}

function sortedWorkspaceFolders(): string[] {
  if (_sortedWorkspaceFolders === void 0) {
    _sortedWorkspaceFolders = workspace.workspaceFolders
      ? workspace.workspaceFolders
          .map((folder) => {
            let result = folder.uri.toString()
            if (result.charAt(result.length - 1) !== '/') {
              result = result + '/'
            }
            return result
          })
          .sort((a, b) => {
            return a.length - b.length
          })
      : []
  }
  return _sortedWorkspaceFolders
}

workspace.onDidChangeWorkspaceFolders(
  () => (_sortedWorkspaceFolders = undefined)
)

function getOuterMostWorkspaceFolder(folder: WorkspaceFolder): WorkspaceFolder {
  let sorted = sortedWorkspaceFolders()
  for (let element of sorted) {
    let uri = folder.uri.toString()
    if (uri.charAt(uri.length - 1) !== '/') {
      uri = uri + '/'
    }
    if (uri.startsWith(element)) {
      return workspace.getWorkspaceFolder(Uri.parse(element))!
    }
  }

  return folder
}

class PikeClient extends LanguageClient {
  public constructor(context: ExtensionContext, config: PikeClientConfig) {
    const serverModule = context.asAbsolutePath(
      path.join('server', 'main.pike')
    )
    const serverModuleOptions = [
      '-M',
      context.asAbsolutePath('pike-modules'),
      serverModule,
    ]

    // If the extension is launched in debug mode then the debug server options
    // are used, otherwise the run options are used
    const serverOptions: ServerOptions = {
      run: {
        command: 'pike',
        args: serverModuleOptions,
        transport: TransportKind.stdio,
      },
      debug: {
        command: 'pike',
        args: serverModuleOptions,
        transport: TransportKind.stdio,
        // NOTE! Pike doesn't support the inspect stuff below yet. Keeping it
        //       as reference
        // --inspect=6009: runs the server in Node's Inspector mode so VS Code
        // can attach to the server for debugging
        options: {
          execArgv: ['--nolazy', '--inspect=6009'],
          env: {
            ...process.env,
            LSP_DEBUG: '1',
          },
        },
      },
    }

    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: 'file', language: 'pike' }],
      synchronize: {
        // Notify the server about file changes to '.clientrc files contained in
        // the workspace
        fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
      },
    }

    clientOptions.initializationOptions = config

    super(
      'pikeLanguageServer',
      'Pike Language Server',
      serverOptions,
      clientOptions
    )

    console.log(
      `This Init Options After Super call: %O`,
      this.clientOptions.initializationOptions
    )
  }
}

export function deactivate(): Thenable<void> | undefined {
  console.log(
    `Deactivating client/s: defaultClient: %o, clients: %o`,
    !!defaultClient,
    clients.size
  )

  defaultClient?.stop()

  for (const cli of Object.values(clients)) {
    cli.stop()
  }

  return undefined
}
