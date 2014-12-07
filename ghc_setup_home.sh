#!/bin/sh -e

#
# Module      : ghc_setup_home.sh
# Description : GHC/Cabal initial setup script for home directory
# Copyright   : (c) notae@me.com, 2014
# License     : BSD-style
# Maintainer  : notae@me.com
# Stability   : experimental
# Portability : POSIX
#
#   This script do the following action:
#     (1) Clean GHC/Cabal directories in your home directory.
#     (2) Install latest cabal from Hackage by cabal in Haskell Platform.
#     (3) Install latest ghc-mod command from Hackage by new cabal.
#
#   Please add the following paths to your $PATH:
#     ${HOME}/Library/Haskell/tools/.cabal-sandbox/bin
#     ${HOME}/Library/Haskell/bin
#

readonly TITLE="GHC/Cabal Setup"
readonly ME=`basename $0`

readonly CP=/bin/cp
readonly RM=/bin/rm
readonly MKDIR=/bin/mkdir

readonly CABAL_SYS=/usr/bin/cabal
readonly CABAL_USR="${HOME}/Library/Haskell/bin/cabal"
readonly CABALG="${HOME}/Library/Haskell/bin/cabalg"
readonly USR_BIN_DIR="${HOME}/Library/Haskell/bin"
readonly DEST_DIRS="${HOME}/.ghc ${HOME}/.cabal ${HOME}/Library/Haskell"
readonly SANDBOX_DIR="${HOME}/Library/Haskell/tools"

export PATH="${USR_BIN_DIR}:/usr/bin:/bin:/usr/sbin:/sbin"

msg ()
{
    echo "================================================================"
    echo ${ME}: $*
    echo "================================================================"
    echo
}

msg "${TITLE} was started."

msg "!! All GHC/Cabal directories in your home will be removed !!"
msg "   Directories: ${DEST_DIRS}"

read -r -p "Are you sure? [y/N] " response
case ${response} in
    [yY][eE][sS]|[yY])
	# go through
	;;
    *)
	msg "${TITLE} was aborted by user."
	exit 255
	;;
esac

msg "Cleaning up old directories ..."
${RM} -rf ${DEST_DIRS}

msg "Setup new Cabal / cabal-install ..."
${CABAL_SYS} update
${CABAL_SYS} install -j Cabal cabal-install
${CABAL_USR} update

msg "Setup utility tools in user directory : cabalg ..."
${CABAL_USR} install -j cabalg

msg "Setup utility library in user directory : ipprint-lite ..."
${CABALG} https://github.com/notae/ipprint.git

msg "Setup utility tools in sandbox : ghc-mod, hlint, etc."
${MKDIR} ${SANDBOX_DIR}
pushd ${SANDBOX_DIR}
${CABAL_USR} sandbox init
${CABAL_USR} install -j ghc-mod hlint doctest stylish-haskell hi present pandoc
popd

msg "${TITLE} was completed."
exit 0
