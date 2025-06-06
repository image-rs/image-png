Copyrights:

* `Lohengrin_-_Illustrated_Sporting_and_Dramatic_News.png`: Public Domain, according to Wikimedia
* `kodim*.png`: Eastman Kodak Company, released for unrestricted use
* `Transparency.png`: Public Domain, according to Wikimedia
* `zune-paletted.png`:
  [Creative Commons Attribution-Share Alike 3.0 Unported](https://creativecommons.org/licenses/by-sa/3.0/deed.en).  This is based on
  https://commons.wikimedia.org/wiki/File:Stadt_Onex_2021.png - it has been
  downloaded at 2048px resolution and modified to use `ColorType::Indexed` with
  `convert -type palette -colors 256 2048px-Stadt_Onex_2021.png
  tests/benches/paletted-zune.png`.
* `tango-icon-address-book-new-32.png`: Public Domain, file from the official Tango icon set distribution
* `tango-icon-address-book-new-128-rsvg-convert.png`: Public Domain, converted from SVG with rsvg-convert
* `lorem_ipsum_screenshot.png`: Public Domain, screenshot of Firefox text rendering on Linux
* `lorem_ipsum_oxipng.png`: Public Domain, the same text screenshot recompressed with `oxipng`
* `Exoplanet_Phase_Curve_(Diagram).png`: Public Domain, raw image from <https://commons.wikimedia.org/wiki/File:Exoplanet_Phase_Curve_(Diagram)_(01HK57P2YHV18MMV0RG5N7HY70).png>
* `Exoplanet_Phase_Curve_(Diagram)_indexed_gimp.png`: Public Domain, the same image converted to indexed in GIMP and saved with compression 9
* `Fantasy_Digital_Painting.png`: CC-BY-SA 3.0, sourced from <https://commons.wikimedia.org/wiki/File:Fantasy_Digital_Painting.png>

The images use different filtering:

* Lohengrin: no filtering
* kodim02: mixed
* kodim07: mainly paeth
* kodim17: mainly sub
* kodim23: mixed
