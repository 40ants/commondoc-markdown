Search.setIndex({"docnames":["../index","../changelog"],"filenames":["../","../changelog/"],"objects":{"COMMONDOC-MARKDOWN/EMITTER":{"*GENERATE-SHORT-LINK-REFERENCES*":[0,22,2,"x-28COMMONDOC-MARKDOWN-2FEMITTER-3A-2AGENERATE-SHORT-LINK-REFERENCES-2A-20-28VARIABLE-29-29"],"*MIN-LINK-HASH-LENGTH*":[0,22,2,"x-28COMMONDOC-MARKDOWN-2FEMITTER-3A-2AMIN-LINK-HASH-LENGTH-2A-20-28VARIABLE-29-29"],"*EMIT-SECTION-ANCHORS*":[0,22,2,"x-28COMMONDOC-MARKDOWN-2FEMITTER-3A-2AEMIT-SECTION-ANCHORS-2A-20-28VARIABLE-29-29"]}},"objnames":[["lisp","symbol","Symbol"],["lisp","argument","Argument"],["lisp","system","ASDF System"],["lisp","class","Class"],["lisp","compiler-macro","Compiler Macro"],["lisp","constant","Constant"],["lisp","function","Function"],["lisp","generic-function","Generic Function"],["lisp","glossary-term","Glossary Term"],["lisp","include","Included Block"],["lisp","stdout-of","Stdout of Code"],["lisp","locative","Locative"],["lisp","macro","Macro"],["lisp","method","Method"],["lisp","package","Package"],["lisp","restart","Restart"],["lisp","section","Section"],["lisp","accessor","Accessor"],["lisp","reader","Slot Reader"],["lisp","writer","Slot Write"],["lisp","structure-accessor","Structure Accessor"],["lisp","type","Type"],["lisp","variable","Variable"]],"objtypes":["lisp:symbol","lisp:argument","lisp:system","lisp:class","lisp:compiler-macro","lisp:constant","lisp:function","lisp:generic-function","lisp:glossary-term","lisp:include","lisp:stdout-of","lisp:locative","lisp:macro","lisp:method","lisp:package","lisp:restart","lisp:section","lisp:accessor","lisp:reader","lisp:writer","lisp:structure-accessor","lisp:type","lisp:variable"],"terms":{"Thi":[0],"is":[1,0],"a":[1,0],"wrapper":[0],"around":[0],"3BMD":[0],"markdown":[0],"parser":[0],"which":[0],"produces\ndocu":[0],"in":[0],"CommonDoc":[0],"format.":[0],"Also,":[0],"it":[1,0],"abl":[0],"to":[1,0],"render\nCommonDoc":[0],"document":[0],"into":[1,0],"the":[1,0],"Markdown.":[1,0],"It":[0],"proof":[0],"of":[0],"concept,":[0],"but":[1,0],"I'm":[0],"alreadi":[0],"us":[0],"builder\n":[0],"40ANTS-DOC":[0],".":[1,0],"Note":[0],"librari":[0],"not":[1,0],"compat":[0],"with":[1,0],"CommonMark":[0],"yet.":[0],"avail":[0],"onli":[0],"at":[0],"Ultralisp.org":[0],"now.":[0],"To":[0],"instal":[0],"Quicklisp":[0],"client,":[0],"do:":[0],"(ql-dist:install-dist":[0],"\"http://dist.ultralisp.org/\"\n":[0],":prompt":[0],"nil)\n\n(ql:quickload":[0],":commondoc-markdown)":[0],"Installat":[0],"CL-USER>":[0],"(common-doc.format:parse-document\n":[0],"(make-inst":[0],"'commondoc-markdown:markdown)\n":[0],"\"\n#":[0],"Hello":[0],"World\n\n*":[0],"First":[0],"item\n*":[0],"Second":[0],"item\n\")\n#<COMMON-DOC:SECTION":[0],"title:":[0],"World,":[0],"ref:":[0],"NIL>":[0],"Pars":[0],"Markdown":[1,0],"Now":[1,0],"we":[0],"can":[1,0],"render":[1,0],"our":[0],"back":[0],"Markdown:":[0],"(common-doc.format:emit-to-string\n":[0],"*document*)\n\"#":[0],"item\n\n\n\"":[0],"Write":[0],"By":[1,0],"default":[1,0],"T":[1,0],",":[1,0],"you":[1,0],"bind":[1,0],"NIL":[1,0],",\nto":[0],"prevent":[1,0],"short":[1,0],"link":[1,0],"refer":[1,0],"generation.":[1,0],"Minumum":[0],"length":[0],"hash":[0],"for":[1,0],"gener":[0],"links.\nThi":[0],"work":[1,0],"when":[0],"*generate-short-link-references*":[0],"variable\ni":[0],"set":[0],"When":[0],"thi":[0],"variabl":[0],"(default),":[0],"emitt":[1,0],"outputs\na":[0],"raw":[0],"html":[0],"<a":[0],"name=\"some-id\"></a>":[0],"befor":[0],"each\nMarkdown":[0],"section.":[0],"API":[0],"Make":[0],"commondoc-markdown":[0],"syntax.":[1,0],"Roadmap":[0],"CommonDoc-Markdown":[0],"Added":[1],"support":[1],"basic":[1],"tabl":[1],"Tabl":[1],"header":[1],"aren't":[1],"yet,":[1],"becaus":[1],"common-doc":[1],"system":[1],"doe":[1],"them":[1],":(":[1],"0.5.0":[1],"(2024-01-27)":[1],"Fix":[1],"latest":[1],"3bmd":[1],"parser.":[1],"wai":[1],"how":[1],"bullet":[1],"list":[1],"ar":[1],"there":[1],"no":[1],"blank":[1],"line":[1],"between":[1],"items.":[1],"0.4.0":[1],"(2023-10-30)":[1],"Parser":[1],"wa":[1],"extend":[1],"produc":[1],"COMMON-DOC:STRIKETHROUGH":[1],"objects\nwhen":[1],"enter":[1],"--text":[1],"surround":[1],"by":[1],"two":[1],"minu":[1],"signs--":[1],"0.3.0":[1],"(2022-03-10)":[1],"Variabl":[1],"commondoc-markdown/emitter:*generate-short-link-references*":[1],"\n":[1],"introduced.":[1],",\n":[1],"HASH-LINK":[1],"function":[1],"now":[1],"export":[1],"from":[1],"package.":[1],"0.2.0":[1],"(2022-03-07)":[1],"Initial":[1],"version":[1],"minim":[1],"docs.":[1],"0.1.0":[1],"(2022-01-25)":[1],"ChangeLog":[1]},"titles":["CommonDoc-Markdown","ChangeLog"],"titleterms":[]})
