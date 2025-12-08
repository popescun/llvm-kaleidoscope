if [ $1 = "--help" ]; then
  echo "usage: ./visualize_cfg <function_name>"
  echo "   <function_name> specifies the name of the function to be visualized."; 
  echo "   The script generates the .dot files for all functions in the input IR file."
  echo "   Currently we need to specify which function should be automatically visualized."
  exit 0
fi

rm *.dot
~/github/llvm-project/llvm/install/bin/llvm-as  < t.ll >> t.as
~/github/llvm-project/llvm/install/bin/opt --passes=dot-cfg t.as
mv .$1.dot t.dot
rm t.as

dot -Tpng t.dot >> t.png && open t.png
