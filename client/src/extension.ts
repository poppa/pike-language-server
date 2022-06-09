import * as path from 'path'
import { workspace, ExtensionContext } from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node'

let client: LanguageClient

export function activate(context: ExtensionContext) {
  console.log(`Activating client: %O`, context)
  const serverModule = context.asAbsolutePath(path.join('server', 'main.pike'))
  const serverModuleOptions = [
    '-M',
    context.asAbsolutePath('pike-modules'),
    serverModule,
  ]

  console.log(`Options: %O\n`, serverModuleOptions)

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

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'pike' }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in
      // the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
    },
  }

  // Create the language client and start the client.
  client = new LanguageClient(
    'pikeLangServer',
    'Pike Language Server',
    serverOptions,
    clientOptions
  )

  // Start the client. This will also launch the server
  void client.start()
}

export function deactivate(): Thenable<void> | undefined {
  console.log(`Deactivating client`)

  if (!client) {
    return undefined
  }

  client.stop()
}
