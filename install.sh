#!/usr/bin/env bash
# Pre Installation script for a GNU Guix system.
# If script has to be re-ran on option 1, delete ~/.ssh/
# If script has to be re-ran on option 2, delete ~/.ssh/config and any copied repos

echo -e "1) Setup SSH Key\n2) Configuration (after SSH Key)\n3) Email Configuration\n4) Profile setup"
echo -n "Enter Number: "
read x

# Options
if [ $((x)) == 1 ]; then
    echo "Creating ssh key"
    ssh-keygen -t rsa -b 4096 -C "x7and7@gmail.com"
    
    echo "Adding SSH Key to agent"
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_rsa

    echo -e "\nNow copy this SSH key to Haider's Git profile:\n"
    sleep 1
    cat ~/.ssh/id_rsa.pub

elif [ $((x)) == 2 ]; then
    echo "Welcome to Haider's Personal Post Installation Script for Guix"
    echo "MAKE SURE YOU HAVE GENERATED A SSH KEY AND ADDED TO HAIDER'S GIT PROFILE"
    sleep 2
    echo "Script starting in two seconds..."
    sleep 2
    echo -e "\nConfiguring git\n"
    touch ~/.ssh/config
    printf "Host github.com\nHostname ssh.github.com\nPort 443">> ~/.ssh/config
    echo "Cloning git servers"
    sleep 1
    cd
    git config --global user.name "Haider Mirza"
    git config --global user.email "x7and7@gmail.com"
    git clone git@github.com:Haider-Mirza/3D-Projects.git
    git clone git@github.com:Haider-Mirza/Passwords.git
    git clone git@github.com:Haider-Mirza/Notes.git
    git clone git@github.com:Haider-Mirza/haider-mirza.github.io.git
    git clone git@github.com:Haider-Mirza/Documents.git
    mkdir ~/code
    cd code
    git clone git@github.com:Haider-Mirza/Spinter.git
    cd ~/dotfiles/guix-home/
    echo -e "\nInstalling all system packages\n"
    guix home reconfigure config.scm
    # guix package -i mpv qutebrowser moc font-fira-code font-jetbrains-mono pandoc slock ksnip blender alacritty emacs picom xmodmap feh ccls gnupg pinentry-tty password-store isync qemu virt-manager dmenu libvirt libvterm alsa-utils node unclutter

    echo -e "\nType '1' if you want to update your system:"

    read y
    if [ $((y)) == 1 ]; then
	guix package -u
	guix pull
    fi
    echo -e "post-installation script complete\nNow tangle all Dotfiles in Emacs then restart it to remove all errors"
    echo "Launching Emacs after three seconds!"
    sleep 1
    echo "Also make sure you remove all dependancies from config.scm after org-roam has been compiled"
    sleep 2
    emacs

elif [ $((x)) == 3 ]; then
    # UNCOMPLETE AREA, Run at your own risk!
    echo -e "MAKE SURE YOU HAVE SETUP PASSWORD STORE\n"
    echo "ALSO MAKE SURE YOU HAVE TANGLED MY EMACS CONFIGURATION AND MBSYNC CONFIGURATION AND RAN ALL PREVIOUS OPTIONS IN THIS SCRIPT"
    echo -e "Script starting in two seconds...\n"

    sleep 2
    guix package -i isync
    mkdir ~/Mail/
    mkdir ~/Mail/Gmail/
    mkdir ~/Mail/Outlook/
    mu init --maildir=~/Mail --my-address=x7and7@gmail.com --my-address=ha6mi19@keaston.bham.sch.uk
    mu index
    echo -e "Syncing Mail\n"
    mbsync -a

elif [ $((x)) == 4 ]; then
    echo -e "MAKE SURE YOU HAVE COPIED OVER MY DOTFILES AS ~/dotfiles\n"
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    mkdir -p "$GUIX_EXTRA_PROFILES"/manifests
    guix package --manifest=$HOME/.guix-extra-profiles/manifests/emacs.scm --profile="$GUIX_EXTRA_PROFILES"/manifests/manifests
    GUIX_PROFILE="$HOME/.guix-extra-profiles/manifests/manifests" ; . "$GUIX_PROFILE/etc/profile"

    bashProfile="$(cat ./profiles/bashprofile)"
    printf "${bashProfile}">> ~/.bash_profile

    cd profiles/
    lsProfiles="$(ls *.scm)"
    cp $lsProfiles $GUIX_EXTRA_PROFILES/manifests

fi
