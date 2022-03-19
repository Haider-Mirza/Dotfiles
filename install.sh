#!/usr/bin/env bash
# Pre Installation script for a GNU Guix system.
# If script has to be re-ran on option 1, delete ~/.ssh/
# If script has to be re-ran on option 2, delete ~/.ssh/config and any copied repos

echo -e "1) Setup SSH Key\n2) Configuration (after SSH Key)"
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
guix package -i git
touch ~/.ssh/config
printf "Host github.com\nHostname ssh.github.com\nPort 443">> ~/.ssh/config
echo "Cloning git servers"
sleep 1
git clone git@github.com:Haider-Mirza/Dotfiles.git
git clone git@github.com:Haider-Mirza/3D-Projects.git
git clone git@github.com:Haider-Mirza/Notes.git
git clone git@github.com:Haider-Mirza/haider-mirza.github.io.git
git clone git@github.com:Haider-Mirza/Documents.git
mkdir ~/code
cd code
git clone git@github.com:Haider-Mirza/Spinter.git
cd
echo -e "\nInstalling final packages\n"
guix package -i mpv qutebrowser moc font-fira-code pandoc 

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
fi
