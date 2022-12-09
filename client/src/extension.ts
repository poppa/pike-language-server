import * as path from 'path'
import { Context } from 'vm'
import {
  ExtensionContext,
  TextDocument,
  Uri,
  workspace,
  WorkspaceFolder,
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

// This is pretty much only for dev purposes
const RunMode: 'pike' | 'ts' = 'ts'

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

      context.subscriptions.push(client)
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

function constructTypescriptServerOptions(context: Context): ServerOptions {
  const serverModule = context.asAbsolutePath(
    path.join('server-ts', 'out', 'server.js')
  )

  const serverOptions: ServerOptions = {
    run: {
      module: serverModule,
      transport: TransportKind.ipc,
    },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: {
        execArgv: ['--nolazy', '--inspect=6009'],
        env: {
          ...process.env,
          LSP_DEBUG: '1',
        },
      },
    },
  }

  return serverOptions
}

function constructPikeNativeServerOptions(context: Context): ServerOptions {
  const serverModule = context.asAbsolutePath(path.join('server', 'main.pike'))

  const serverModuleOptions = [
    '-DPLS_LSP_DEBUG',
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

  return serverOptions
}

class PikeClient extends LanguageClient {
  public constructor(context: ExtensionContext, config: PikeClientConfig) {
    const serverOptions =
      RunMode === 'ts'
        ? constructTypescriptServerOptions(context)
        : constructPikeNativeServerOptions(context)

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

  public async dispose(): Promise<void> {
    console.log(`** Got dispose in client`)
    if (this.isRunning()) {
      await this.stop()
    }
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
