if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x SKIM_DEFAULT_OPTIONS "$SKIM_DEFAULT_OPTIONS \
--color=fg:#c6d0f5,bg:#303446,matched:#414559,matched_bg:#eebebe,current:#c6d0f5,current_bg:#51576d,current_match:#303446,current_match_bg:#f2d5cf,spinner:#a6d189,info:#ca9ee6,prompt:#8caaee,cursor:#e78284,selected:#ea999c,header:#81c8be,border:#737994"

    starship init fish | source
end

