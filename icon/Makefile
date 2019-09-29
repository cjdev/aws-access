all: icon.iconset
	iconutil -c icns icon.iconset 

icon.iconset: icon.svg
	rm -rf icon.iconset
	mkdir -p icon.iconset
	rsvg-convert -w 16   -h 16   icon.svg > icon.iconset/icon_16x16.png
	rsvg-convert -w 32   -h 32   icon.svg > icon.iconset/icon_16x16@2x.png
	rsvg-convert -w 32   -h 32   icon.svg > icon.iconset/icon_32x32.png
	rsvg-convert -w 64   -h 64   icon.svg > icon.iconset/icon_32x32@2x.png
	rsvg-convert -w 64   -h 64   icon.svg > icon.iconset/icon_64x64.png
	rsvg-convert -w 128	-h 128	icon.svg > icon.iconset/icon_64x64@2x.png
	rsvg-convert -w 128	-h 128	icon.svg > icon.iconset/icon_128x128.png
	rsvg-convert -w 256	-h 256	icon.svg > icon.iconset/icon_128x128@2x.png
	rsvg-convert -w 256	-h 256	icon.svg > icon.iconset/icon_256x256.png
	rsvg-convert -w 512	-h 512	icon.svg > icon.iconset/icon_256x256@2x.png
	rsvg-convert -w 512	-h 512	icon.svg > icon.iconset/icon_512x512.png
	rsvg-convert -w 1024	-h 1024	icon.svg > icon.iconset/icon_512x512@2x.png
	rsvg-convert -w 1024	-h 1024	icon.svg > icon.iconset/icon_1024x1024.png
	rsvg-convert -w 2048	-h 2048	icon.svg > icon.iconset/icon_1024x1024@2x.png
