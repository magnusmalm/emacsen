#!/usr/bin/env bash

# This script will install my Emacs configuration.
# It must be run from the root of the cloned repo.
#
# Steps:
#
# 1) git clone https://github.com/magnusmalm/emacs.git ~/emacsen
# _OR_
# 1) git clone git@github.com:magnusmalm/emacs.git ~/emacsen
# 2) cd ~/emacsen
# 3) ./install-emacs-config.sh
#
#

set -euxo pipefail

chemacs_dir=~/src/chemacs

if [[ ! -d common ]] ; then
    printf "The common directory does not exist!\n"
    printf "Verify that the git clone was ok and that you are in the correct directory.\n"
    exit 1
fi

if [[ ! -f .emacs-profiles.el ]] ; then
    printf "The .emacs-profiles.el file does not exist!\n"
    printf "Verify that the git clone was ok and that you are in the correct directory.\n"
    exit 1
fi

if [[ -d "$chemacs_dir" ]] ; then
    printf "Chemacs already cloned in %s. Doing pull.\n" "$chemacs_dir"
    pushd "$chemacs_dir"
    git pull
    if [[ "$?" != "0" ]] ; then
	printf "Pull of chemacs failed. Bailing out.\n"
	exit 1
    fi
else
    git clone https://github.com/plexus/chemacs.git "$chemacs_dir"
    if [[ "$?" != "0" ]] ; then
	printf "Clone of chemacs failed. Bailing out.\n"
	exit 1
    fi
    pushd "$chemacs_dir"
fi

if [[ -f ~/.emacs ]] ; then
    tmp=~/.emacs-$(date +%Y-%m-%d-%H%M%S)
    printf "Backing up ~/.emacs to %s\n" $"tmp"
    mv -v ~/.emacs "$tmp"
fi
./install.sh
popd

if [[ -f ~/.emacs-profiles.el ]] ; then
    tmp=~/.emacs-profiles.el-$(date +%Y-%m-%d-%H%M%S)
    printf "Backing up ~/.emacs-progfiles.el to %s\n" "$tmp"
    mv -v ~/.emacs "$tmp"
fi

ln -vs "$(pwd)/".emacs-profiles.el ~/
mkdir -v common/straight

for d in devel org irc lisp; do
    if [[ -d "$d" ]] ; then
	tmp="$d"-$(date +%Y-%m-%d-%H%M%S)
	printf "Backing up %s to %s\n" "$d" "$tmp"
	mv -v "$d" "$tmp"
    fi
    mkdir "$d"
    for l in config init.el lisp straight; do
	ln -vs "$(pwd)"/common/"$l" "$d"
    done
done

tree -d -L 2
ls -l ~/.emacs ~/.emacs-profiles.el
