STATUS=`defaults read com.apple.finder AppleShowAllFiles`
if [ $STATUS == 1 ]
then defaults write com.apple.finder AppleShowAllFiles -boolean FALSE
else defaults write com.apple.finder AppleShowAllFiles -boolean TRUE
fi
killall Finder
