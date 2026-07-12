_tokens89() {
    local current command
    current="${COMP_WORDS[COMP_CWORD]}"
    command="${COMP_WORDS[1]}"
    if [[ $COMP_CWORD -eq 1 ]]; then
        COMPREPLY=( $(compgen -W 'encode decode inspect tokenize detokenize verify' -- "$current") )
        return
    fi
    case "$command" in
        encode) COMPREPLY=( $(compgen -W '-o --output --name --folder --type --no-tokenize --force' -- "$current") ) ;;
        decode) COMPREPLY=( $(compgen -W '-o --output --force' -- "$current") ) ;;
        tokenize) COMPREPLY=( $(compgen -W '-o --output --type --hex --no-tokenize --force' -- "$current") ) ;;
        detokenize) COMPREPLY=( $(compgen -W '-o --output --hex --force' -- "$current") ) ;;
    esac
}
complete -F _tokens89 tokens89
