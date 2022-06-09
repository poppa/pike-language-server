([ /* 4 elements */
  "id": 0,
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": ([ /* 8 elements */
      "capabilities": ([ /* 5 elements */
          "general": ([ /* 4 elements */
              "markdown": ([ /* 2 elements */
                  "parser": "marked",
                  "version": "1.1.0"
                ]),
              "positionEncodings": ({ /* 1 element */
                    "utf-16"
                }),
              "regularExpressions": ([ /* 2 elements */
                  "engine": "ECMAScript",
                  "version": "ES2020"
                ]),
              "staleRequestSupport": ([ /* 2 elements */
                  "cancel": Val.true,
                  "retryOnContentModified": ({ /* 3 elements */
                        "textDocument/semanticTokens/full",
                        "textDocument/semanticTokens/range",
                        "textDocument/semanticTokens/full/delta"
                    })
                ])
            ]),
          "notebookDocument": ([ /* 1 element */
              "synchronization": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "executionSummarySupport": Val.true
                ])
            ]),
          "textDocument": ([ /* 29 elements */
              "callHierarchy": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "codeAction": ([ /* 7 elements */
                  "codeActionLiteralSupport": ([ /* 1 element */
                      "codeActionKind": ([ /* 1 element */
                          "valueSet": ({ /* 8 elements */
                                "",
                                "quickfix",
                                "refactor",
                                "refactor.extract",
                                "refactor.inline",
                                "refactor.rewrite",
                                "source",
                                "source.organizeImports"
                            })
                        ])
                    ]),
                  "dataSupport": Val.true,
                  "disabledSupport": Val.true,
                  "dynamicRegistration": Val.true,
                  "honorsChangeAnnotations": Val.false,
                  "isPreferredSupport": Val.true,
                  "resolveSupport": ([ /* 1 element */
                      "properties": ({ /* 1 element */
                            "edit"
                        })
                    ])
                ]),
              "codeLens": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "colorProvider": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "completion": ([ /* 6 elements */
                  "completionItem": ([ /* 10 elements */
                      "commitCharactersSupport": Val.true,
                      "deprecatedSupport": Val.true,
                      "documentationFormat": ({ /* 2 elements */
                            "markdown",
                            "plaintext"
                        }),
                      "insertReplaceSupport": Val.true,
                      "insertTextModeSupport": ([ /* 1 element */
                          "valueSet": ({ /* 2 elements */
                                1,
                                2
                            })
                        ]),
                      "labelDetailsSupport": Val.true,
                      "preselectSupport": Val.true,
                      "resolveSupport": ([ /* 1 element */
                          "properties": ({ /* 3 elements */
                                "documentation",
                                "detail",
                                "additionalTextEdits"
                            })
                        ]),
                      "snippetSupport": Val.true,
                      "tagSupport": ([ /* 1 element */
                          "valueSet": ({ /* 1 element */
                                1
                            })
                        ])
                    ]),
                  "completionItemKind": ([ /* 1 element */
                      "valueSet": ({ /* 25 elements */
                            1,
                            2,
                            3,
                            4,
                            5,
                            6,
                            7,
                            8,
                            9,
                            10,
                            11,
                            12,
                            13,
                            14,
                            15,
                            16,
                            17,
                            18,
                            19,
                            20,
                            21,
                            22,
                            23,
                            24,
                            25
                        })
                    ]),
                  "completionList": ([ /* 1 element */
                      "itemDefaults": ({ /* 4 elements */
                            "commitCharacters",
                            "editRange",
                            "insertTextFormat",
                            "insertTextMode"
                        })
                    ]),
                  "contextSupport": Val.true,
                  "dynamicRegistration": Val.true,
                  "insertTextMode": 2
                ]),
              "declaration": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "linkSupport": Val.true
                ]),
              "definition": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "linkSupport": Val.true
                ]),
              "diagnostic": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "relatedDocumentSupport": Val.false
                ]),
              "documentHighlight": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "documentLink": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "tooltipSupport": Val.true
                ]),
              "documentSymbol": ([ /* 5 elements */
                  "dynamicRegistration": Val.true,
                  "hierarchicalDocumentSymbolSupport": Val.true,
                  "labelSupport": Val.true,
                  "symbolKind": ([ /* 1 element */
                      "valueSet": ({ /* 26 elements */
                            1,
                            2,
                            3,
                            4,
                            5,
                            6,
                            7,
                            8,
                            9,
                            10,
                            11,
                            12,
                            13,
                            14,
                            15,
                            16,
                            17,
                            18,
                            19,
                            20,
                            21,
                            22,
                            23,
                            24,
                            25,
                            26
                        })
                    ]),
                  "tagSupport": ([ /* 1 element */
                      "valueSet": ({ /* 1 element */
                            1
                        })
                    ])
                ]),
              "foldingRange": ([ /* 5 elements */
                  "dynamicRegistration": Val.true,
                  "foldingRange": ([ /* 1 element */
                      "collapsedText": Val.false
                    ]),
                  "foldingRangeKind": ([ /* 1 element */
                      "valueSet": ({ /* 3 elements */
                            "comment",
                            "imports",
                            "region"
                        })
                    ]),
                  "lineFoldingOnly": Val.true,
                  "rangeLimit": 5000
                ]),
              "formatting": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "hover": ([ /* 2 elements */
                  "contentFormat": ({ /* 2 elements */
                        "markdown",
                        "plaintext"
                    }),
                  "dynamicRegistration": Val.true
                ]),
              "implementation": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "linkSupport": Val.true
                ]),
              "inlayHint": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "resolveSupport": ([ /* 1 element */
                      "properties": ({ /* 5 elements */
                            "tooltip",
                            "textEdits",
                            "label.tooltip",
                            "label.location",
                            "label.command"
                        })
                    ])
                ]),
              "inlineValue": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "linkedEditingRange": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "onTypeFormatting": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "publishDiagnostics": ([ /* 5 elements */
                  "codeDescriptionSupport": Val.true,
                  "dataSupport": Val.true,
                  "relatedInformation": Val.true,
                  "tagSupport": ([ /* 1 element */
                      "valueSet": ({ /* 2 elements */
                            1,
                            2
                        })
                    ]),
                  "versionSupport": Val.false
                ]),
              "rangeFormatting": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "references": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "rename": ([ /* 4 elements */
                  "dynamicRegistration": Val.true,
                  "honorsChangeAnnotations": Val.true,
                  "prepareSupport": Val.true,
                  "prepareSupportDefaultBehavior": 1
                ]),
              "selectionRange": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "semanticTokens": ([ /* 9 elements */
                  "augmentsSyntaxTokens": Val.true,
                  "dynamicRegistration": Val.true,
                  "formats": ({ /* 1 element */
                        "relative"
                    }),
                  "multilineTokenSupport": Val.false,
                  "overlappingTokenSupport": Val.false,
                  "requests": ([ /* 2 elements */
                      "full": ([ /* 1 element */
                          "delta": Val.true
                        ]),
                      "range": Val.true
                    ]),
                  "serverCancelSupport": Val.true,
                  "tokenModifiers": ({ /* 10 elements */
                        "declaration",
                        "definition",
                        "readonly",
                        "static",
                        "deprecated",
                        "abstract",
                        "async",
                        "modification",
                        "documentation",
                        "defaultLibrary"
                    }),
                  "tokenTypes": ({ /* 23 elements */
                        "namespace",
                        "type",
                        "class",
                        "enum",
                        "interface",
                        "struct",
                        "typeParameter",
                        "parameter",
                        "variable",
                        "property",
                        "enumMember",
                        "event",
                        "function",
                        "method",
                        "macro",
                        "keyword",
                        "modifier",
                        "comment",
                        "string",
                        "number",
                        "regexp",
                        "operator",
                        "decorator"
                    })
                ]),
              "signatureHelp": ([ /* 3 elements */
                  "contextSupport": Val.true,
                  "dynamicRegistration": Val.true,
                  "signatureInformation": ([ /* 3 elements */
                      "activeParameterSupport": Val.true,
                      "documentationFormat": ({ /* 2 elements */
                            "markdown",
                            "plaintext"
                        }),
                      "parameterInformation": ([ /* 1 element */
                          "labelOffsetSupport": Val.true
                        ])
                    ])
                ]),
              "synchronization": ([ /* 4 elements */
                  "didSave": Val.true,
                  "dynamicRegistration": Val.true,
                  "willSave": Val.true,
                  "willSaveWaitUntil": Val.true
                ]),
              "typeDefinition": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "linkSupport": Val.true
                ]),
              "typeHierarchy": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ])
            ]),
          "window": ([ /* 3 elements */
              "showDocument": ([ /* 1 element */
                  "support": Val.true
                ]),
              "showMessage": ([ /* 1 element */
                  "messageActionItem": ([ /* 1 element */
                      "additionalPropertiesSupport": Val.true
                    ])
                ]),
              "workDoneProgress": Val.true
            ]),
          "workspace": ([ /* 14 elements */
              "applyEdit": Val.true,
              "codeLens": ([ /* 1 element */
                  "refreshSupport": Val.true
                ]),
              "configuration": Val.true,
              "diagnostics": ([ /* 1 element */
                  "refreshSupport": Val.true
                ]),
              "didChangeConfiguration": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "didChangeWatchedFiles": ([ /* 2 elements */
                  "dynamicRegistration": Val.true,
                  "relativePatternSupport": Val.true
                ]),
              "executeCommand": ([ /* 1 element */
                  "dynamicRegistration": Val.true
                ]),
              "fileOperations": ([ /* 7 elements */
                  "didCreate": Val.true,
                  "didDelete": Val.true,
                  "didRename": Val.true,
                  "dynamicRegistration": Val.true,
                  "willCreate": Val.true,
                  "willDelete": Val.true,
                  "willRename": Val.true
                ]),
              "inlayHint": ([ /* 1 element */
                  "refreshSupport": Val.true
                ]),
              "inlineValue": ([ /* 1 element */
                  "refreshSupport": Val.true
                ]),
              "semanticTokens": ([ /* 1 element */
                  "refreshSupport": Val.true
                ]),
              "symbol": ([ /* 4 elements */
                  "dynamicRegistration": Val.true,
                  "resolveSupport": ([ /* 1 element */
                      "properties": ({ /* 1 element */
                            "location.range"
                        })
                    ]),
                  "symbolKind": ([ /* 1 element */
                      "valueSet": ({ /* 26 elements */
                            1,
                            2,
                            3,
                            4,
                            5,
                            6,
                            7,
                            8,
                            9,
                            10,
                            11,
                            12,
                            13,
                            14,
                            15,
                            16,
                            17,
                            18,
                            19,
                            20,
                            21,
                            22,
                            23,
                            24,
                            25,
                            26
                        })
                    ]),
                  "tagSupport": ([ /* 1 element */
                      "valueSet": ({ /* 1 element */
                            1
                        })
                    ])
                ]),
              "workspaceEdit": ([ /* 5 elements */
                  "changeAnnotationSupport": ([ /* 1 element */
                      "groupsOnLabel": Val.true
                    ]),
                  "documentChanges": Val.true,
                  "failureHandling": "textOnlyTransactional",
                  "normalizesLineEndings": Val.true,
                  "resourceOperations": ({ /* 3 elements */
                        "create",
                        "rename",
                        "delete"
                    })
                ]),
              "workspaceFolders": Val.true
            ])
        ]),
      "clientInfo": ([ /* 2 elements */
          "name": "Visual Studio Code",
          "version": "1.67.2"
        ]),
      "locale": "en-gb",
      "processId": 55760,
      "rootPath": "/Users/pontus",
      "rootUri": "file:///Users/pontus",
      "trace": "off",
      "workspaceFolders": ({ /* 1 element */
            ([ /* 2 elements */
              "name": "pontus",
              "uri": "file:///Users/pontus"
            ])
        })
    ])
])
