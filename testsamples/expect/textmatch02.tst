# the line break in  otherworld.txt and otherworld2.txt are expected to be different
# LF CR vs LF only
# those should match textually and return Success
expect TextMatch('otherworld.txt', 'otherworld2.txt')