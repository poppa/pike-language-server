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

    // Untitled files go to a default client.
    if (!folder || uri.scheme === 'untitled') {
      console.log(`Untitled or no Folder`)
      if (!defaultClient) {
        console.log(
          `create default client: Workspace conf: %O`,
          workspace.getConfiguration().get('[pike]')
        )
        defaultClient = new PikeClient(context)
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
      let client = new PikeClient(context)
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
  public constructor(context: ExtensionContext) {
    const serverModule = context.asAbsolutePath(
      path.join('server', 'main.pike')
    )
    const serverModuleOptions = [
      '-M',
      context.asAbsolutePath('pike-modules'),
      serverModule,
    ]

    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can
    // attach to the server for debugging
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] }

    // If the extension is launched in debug mode then the debug server options
    // are used, otherwise the run options are used
    const serverOptions: ServerOptions = {
      run: {
        // @ts-expect-error
        command: 'pike',
        args: serverModuleOptions,
        transport: TransportKind.stdio,
      },
      debug: {
        // @ts-expect-error
        command: 'pike',
        args: serverModuleOptions,
        transport: TransportKind.stdio,
        options: debugOptions,
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

    super(
      'pikeLanguageServer',
      'Pike Language Server',
      serverOptions,
      clientOptions
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
