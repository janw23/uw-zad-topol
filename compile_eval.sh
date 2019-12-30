cd src
ocamlopt pMap.mli pMap.ml topol.mli topol.ml -o ../build/eval
cd ..
bash clean_src.sh
