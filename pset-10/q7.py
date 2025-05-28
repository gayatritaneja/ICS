from PIL import Image
import math

def main():
    img = Image.open("nice_image.jpg").convert("RGB")
    s = 1/2

    pixels = list(img.getdata())
    width, height = img.size

    pixel_matrix = [pixels[i * width:(i + 1) * width] for i in range(height)]

    save_image(scale_image(pixel_matrix, s))


def scale_image(image, s):
    og_height = len(image)
    og_width = len(image[0])

    new_height = int(og_height * s)
    new_width = int(og_width * s)

    original_height = len(image)
    original_width = len(image[0])
    

    if new_height == 1:
        ratio_y = 0
    else:
        ratio_y = (original_height - 1) / (new_height - 1)

    if new_width == 1:
        ratio_x = 0
    else:
        ratio_x = (original_width - 1) / (new_width - 1)

    new_image = []

    for i in range(new_height):
        row = []
        for j in range(new_width):
            y = ratio_y * i
            x = ratio_x * j

            y_l = int(math.floor(y))
            y_h = int(math.ceil(y))
            x_l = int(math.floor(x))
            x_h = int(math.ceil(x))

            wy = y - y_l
            wx = x - x_l

            y_l = clamp(y_l, 0, original_height - 1)
            y_h = clamp(y_h, 0, original_height - 1)
            x_l = clamp(x_l, 0, original_width - 1)
            x_h = clamp(x_h, 0, original_width - 1)

            A = image[y_l][x_l]
            B = image[y_l][x_h]
            C = image[y_h][x_l]
            D = image[y_h][x_h]

            r = (A[0] * (1 - wx) + B[0] * wx) * (1 - wy) + (C[0] * (1 - wx) + D[0] * wx) * wy
            g = (A[1] * (1 - wx) + B[1] * wx) * (1 - wy) + (C[1] * (1 - wx) + D[1] * wx) * wy
            b = (A[2] * (1 - wx) + B[2] * wx) * (1 - wy) + (C[2] * (1 - wx) + D[2] * wx) * wy

            row.append((int(round(r)), int(round(g)), int(round(b))))
        new_image.append(row)

    return new_image

def clamp(x, low, high):
    return (max(low, min(x, high)))

def save_image(new_img, filename="scaled_image.jpg"):
    height = len(new_img)
    width = len(new_img[0])
    flat_pixels = [
        (int(r), int(g), int(b))
        for row in new_img for (r, g, b) in row
    ]

    output_img = Image.new("RGB", (width, height))
    output_img.putdata(flat_pixels)
    output_img.save(filename)
    print(f"Saved scaled image as '{filename}'")



main()