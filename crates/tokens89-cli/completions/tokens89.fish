complete -c tokens89 -f
complete -c tokens89 -n '__fish_use_subcommand' -a encode -d 'Encode source into a TI variable file'
complete -c tokens89 -n '__fish_use_subcommand' -a decode -d 'Decode a TI variable file'
complete -c tokens89 -n '__fish_use_subcommand' -a inspect -d 'Display TI variable metadata'
complete -c tokens89 -n '__fish_use_subcommand' -a tokenize -d 'Convert source to raw tokens'
complete -c tokens89 -n '__fish_use_subcommand' -a detokenize -d 'Convert raw tokens to source'
complete -c tokens89 -n '__fish_use_subcommand' -a verify -d 'Verify a TI variable container'
complete -c tokens89 -n '__fish_seen_subcommand_from encode tokenize' -l type -r
complete -c tokens89 -n '__fish_seen_subcommand_from encode' -l name -r
complete -c tokens89 -n '__fish_seen_subcommand_from encode' -l folder -r
complete -c tokens89 -n '__fish_seen_subcommand_from encode tokenize' -l no-tokenize
complete -c tokens89 -n '__fish_seen_subcommand_from tokenize detokenize' -l hex
complete -c tokens89 -n '__fish_seen_subcommand_from encode decode tokenize detokenize' -s o -l output -r
complete -c tokens89 -n '__fish_seen_subcommand_from encode decode tokenize detokenize' -l force
