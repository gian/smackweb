SMLCGIC=/home/gdpe/.smackage/bin/smlcgic
MLTON=mlton

all: index.cgi

index.cgi: content.cgi.sml smackweb.sml
	$(MLTON) -output index.cgi smackweb.mlb

content.cgi.sml: content.mlt
	$(SMLCGIC) content.mlt

install: index.cgi
	cp index.cgi /home/gdpe/public_html/smackage/cgi-bin

