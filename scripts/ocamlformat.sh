changed=$(git diff --name-only origin/dev --diff-filter=ACM | grep 'src/' | grep '\.ml$\|\.mli$')
exit_code=0
for i in $changed; do
    if ! $(ocamlformat --check $i); then
        exit_code=1
        echo -e "\033[31mNot correctly formatted:\033[0m $i."
    fi
done

if [[ $exit_code -eq 1 ]]; then 
   echo ""
   echo "Solve incorrect formatting by:"
   echo -e " * running \033[1m'ocamlformat <filename> --inplace'\033[0m to correct the formatting"
   echo ""
   echo "If that isn't sufficient: see: https://github.com/ocaml-ppx/ocamlformat for more options"
fi;
exit $exit_code