~/github/llvm-project/llvm/install/bin/llvm-as  < t.ll >> t.as
~/github/llvm-project/llvm/install/bin/opt --passes=dot-callgraph t.as >> t.dot 
mv t.as.callgraph.dot t.dot
rm t.as

dot -Tpng t.dot >> t.png && open t.png
