#!/bin/sh

STATE=$(nmcli networking connectivity)
MAILDIR_PATH=$HOME/Mail
# TRASH_FOLDER=gmail/Trash
# ALL_FOLDER=gmail/All

if [ $STATE = 'full' ]; then
    ##
    # pre-new
    #

    # tag trashing
    notmuch tag +trashing -- $(notmuch search --output=threads \(tag:trashing or tag:deleted\) and from:/sorend\?/)
    # Move tag:trashing to the folder (there's no convenient way to move) and replace with tag:trashed
    # notmuch search --format=text0 --output=files tag:trashing and not folder:$TRASH_FOLDER | xargs -0 -n1 -I {} mv "{}" $MAILDIR_PATH/$TRASH_FOLDER/cur/
    notmuch tag -trashing +trashed -- tag:trashing or folder:$TRASH_FOLDER
    notmuch search --format=text0 --output=files tag:killing | xargs -0 -n1 -I {} mv {} rm

    # inbox cleanup (move non-inbox to all folder except gmail/INBOX which is already part of All)
    notmuch search --format=text0 --output=files folder:/INBOX/ and not tag:inbox and not folder:gmail/INBOX | xargs -0 -n1 -I {} mv "{}" $MAILDIR_PATH/$ALL_FOLDER/cur/

    ##
    # sync and update index
    #
    
    # sync imap
    mbsync -a -c $HOME/.emacs.d/mail/mbsync.conf

    # update index with new mails
    notmuch new

    ##
    # post-new
    #
    
    # tagging
    notmuch tag -inbox -unread -new -touched +trashed -- folder:/Trash/
    notmuch tag -inbox -unread -new -touched +spam -- folder:/Spam/
    notmuch tag -inbox -unread -new +sent -draft -- folder:/Sent/
    notmuch tag -inbox -unread -new +draft -- folder:/Draft/

    # mailers
    notmuch tag -inbox -new +mailer/quora -- from:/@quora.com/ and tag:new
    notmuch tag -inbox -new +mailer/amazon -- from:/@amazon.in/ and tag:new
    notmuch tag -inbox -new +mailer/bank +hdfc -- from:alerts@hdfcbank.net and tag:new
    notmuch tag -inbox -new +mailer/bank +niyox -- from:esfb-alerts@equitasbank.com and tag:new
    notmuch tag -inbox -new +mailer/research +academia -- from:updates@academia-mail.com and tag:new
    notmuch tag -inbox -new +mailer/bank +twino -- from:info@twinoinvest.eu and tag:new

    # inbox
    notmuch tag +inbox -- folder:/INBOX/ and tag:new
    notmuch tag +trashing -- tag:newsletter and date:..3-months-ago

    # finished
    notmuch tag -new -- tag:new
    exit 0
fi
echo "No Internets!"
exit 0
