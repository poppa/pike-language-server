import {
  workspace as Workspace,
  ExtensionContext,
  window as Window,
  TextDocument,
  OutputChannel,
  WorkspaceFolder,
  Uri,
  workspace,
  languages,
  DocumentSelector,
} from 'vscode'

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node'

let defaultClient: LanguageClient
let clients: Map<string, LanguageClient> = new Map()
let _sortedWorkspaceFolders: string[] | undefined
let _serverPath = 'org.gnome.gvls.stdio.Server'

export function activate(context: ExtensionContext) {
  function didOpenTextDocument(document: TextDocument): void {
    // We are only interested in language mode text
    if (
      document.languageId !== 'vala' ||
      (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')
    ) {
      return
    }

    // Generate ValaConfiguration settings
    let conf = new ValaConfiguration()

    let uri = document.uri
    // Untitled files go to a default client.
    if (uri.scheme === 'untitled' && !defaultClient) {
      workspace.getConfiguration()
      defaultClient = new GVlsClient(_serverPath, conf)
      defaultClient.start()
      return
    }
    let folder = Workspace.getWorkspaceFolder(uri)
    // Files outside a folder can't be handled. This might depend on the language.
    // Single file languages like JSON might handle files outside the workspace folders.
    if (!folder) {
      return
    }
    // If we have nested workspace folders we only start a server on the outer most workspace folder.
    folder = getOuterMostWorkspaceFolder(folder)

    if (!clients.has(folder.uri.toString())) {
      let client = new GVlsClient(_serverPath, conf)
      client.start()
      clients.set(folder.uri.toString(), client)
    }
  }

  Workspace.onDidOpenTextDocument(didOpenTextDocument)
  Workspace.textDocuments.forEach(didOpenTextDocument)
  Workspace.onDidChangeWorkspaceFolders((event) => {
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
    _sortedWorkspaceFolders = Workspace.workspaceFolders
      ? Workspace.workspaceFolders
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

Workspace.onDidChangeWorkspaceFolders(
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
      return Workspace.getWorkspaceFolder(Uri.parse(element))!
    }
  }
  return folder
}

export class GVlsClient extends LanguageClient {
  constructor(serverPath: string, conf: ValaConfiguration) {
    let serverModule = serverPath

    if (serverModule == null) return
    let clientOptions: LanguageClientOptions = {
      documentSelector: ['vala'],
    }

    let serverOptions: ServerOptions = {
      run: {
        command: serverModule,
        transport: TransportKind.stdio,
      },
      debug: {
        command: serverModule,
        options: {
          env: {
            ...process.env,
            G_MESSAGES_DEBUG: 'all',
            JSONRPC_DEBUG: 1,
          },
        },
        transport: TransportKind.stdio,
      },
    }

    super('gvls', 'GNOME Vala Language Server', serverOptions, clientOptions)

    this.clientOptions.initializationOptions = conf
  }

  dispose() {
    // stop()
  }
}

class ValaConfiguration {
  // initialized: true
  // defaultNamespaces: false
  // defaultVapiDirs: true
  // scanWorkspace: true
  // addUsingNamespaces: true
  // mesonBuildSystem: true
  // libraryVapi: ''
  // systemVapi: ''
  // files: []
  // packages: []
  // valaArgs: []
  // options: []
}
