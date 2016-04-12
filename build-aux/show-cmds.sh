##  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
##  Copyright (C) 2015
##      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
##  Artanis is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License and GNU
##  Lesser General Public License published by the Free Software
##  Foundation, either version 3 of the License, or (at your option)
##  any later version.

##  Artanis is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License and GNU Lesser General Public License
##  for more details.

##  You should have received a copy of the GNU General Public License
##  and GNU Lesser General Public License along with this program.
##  If not, see <http://www.gnu.org/licenses/>.

# install in /etc/bash_completion.d/ or your personal directory

_art()
{
    local cur=`_get_cword`
    local cmds=`art list-all-cmds`
    # TODO: support cmd subcmd & options complete 
    #local cmd="${COMP_WORDS[1]}"
    
    if [ "$COMP_CWORD" == 1 ]; then
        for opt in $cmds; do
            if [[ "$opt" == "$cmd" ]]; then
                COMPREPLY=("$opt")
                return
            fi
        done
    elif [ "$COMP_CWORD" == 2 ]; then
        # TODO: support cmd options complete
        return
    fi

    COMPREPLY=($(compgen -W "$cmds" -- "$cur"))
    return
}

complete $filenames -F _art art

# vim:ts=2 sw=2 et syn=sh
