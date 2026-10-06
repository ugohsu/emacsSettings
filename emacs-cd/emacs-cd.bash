# emacs-cd: emacs -nw で開き、終了したときの場所に cd する (ranger-cd にならう)
# ~/.bashrc から読み込む:
#   . /path/to/emacsSettings/emacs-cd/emacs-cd.bash
# 引数を省くと今のディレクトリを dired で開く。引数はそのまま emacs に渡す

_emacs_cd_el="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/emacs-cd.el"

emacs-cd() {
    local dir_file dir
    dir_file=$(mktemp) || return
    EMACS_CD_FILE="$dir_file" emacs -nw -l "$_emacs_cd_el" "${@:-.}"
    dir=$(cat "$dir_file")
    rm -f "$dir_file"
    if [ -n "$dir" ] && [ -d "$dir" ]; then
        cd -- "$dir" || return
    fi
}

# C-x e で emacs-cd を起動する (bash 標準の call-last-kbd-macro を上書き)
if [[ $- == *i* ]]; then
    bind '"\C-xe":"emacs-cd\C-m"'
fi
