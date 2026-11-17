"""Shrink dashboard screenshots: quantize to a 256-color palette (the dark UI uses few
colors, so this is visually lossless at README size) and re-save optimized.

Usage: python3 scripts/compress_png.py docs/assets/*.png   (needs Pillow)
"""
import os
import sys

from PIL import Image

for path in sys.argv[1:]:
    before = os.path.getsize(path)
    image = Image.open(path).convert("RGB")
    image.quantize(colors=256, method=Image.Quantize.MEDIANCUT, dither=Image.Dither.NONE).save(
        path, optimize=True
    )
    print(f"{path}: {before // 1024} KB -> {os.path.getsize(path) // 1024} KB")
