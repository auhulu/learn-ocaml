# プログラミングの基礎（浅井健一）勉強用
## 環境構築
```bash
#WSL2+Ubunutu 20.04  
$ sudo apt update  
$ sudo apt upgrade  
$ sudo apt install build-essential git mercurial rsync darcs bubblewrap  
$ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)  
$ opam init  
$ eval $(opam env)  
#OCamlのバージョンを確認(今回はOCaml version 4.12.1)  
$ opam switch  
$ opam install dune merlin ocp-indent ocamlformat utop  
#これで対話環境が立ち上がるがカーソルキーが効かない
$ ocaml  
#rlwrap コマンドは行入力においてヒストリ機能などがないコマンドにreadline 相当の機能を後付け   
$ sudo install rlwrap  
$ rlwrap ocaml  
#プログラムの実行  
use "プログラム名" ;;  
#終了  
exit 0;;
```

## 分割コンパイル
- OCamlMakefileはここで入手  
https://github.com/mmottl/ocaml-makefile/blob/master/OCamlMakefile
- ディレクトリにOCamlMakefileとMakefileを配置しmake topでhoge.topが生成
- ./hoge.topで実行
- del.shで余計な諸々を一括で削除