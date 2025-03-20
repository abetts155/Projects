import pygame
import sys

from model import pitches
from model.pitches import Coordinate, Rectangle, PitchSide, Post, Pitch


GREEN = (123, 176, 44)
WHITE = (255, 255, 255)


def main():
    pygame.init()

    pygame.display.set_caption('Frantic Football')

    SCREEN_WIDTH = 1600
    SCREEN_HEIGHT = 1000
    DISPLAY = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
    DISPLAY.fill(GREEN)

    pitch = Pitch()
    percentage = 0.9
    width_scale = percentage * SCREEN_WIDTH / pitch.field.width
    height_scale = percentage * SCREEN_HEIGHT / pitch.field.height
    scaled_width = round(width_scale * pitch.field.width)
    scaled_height = round(height_scale * pitch.field.height)

    horizontal_offset = (SCREEN_WIDTH - scaled_width) // 2
    vertical_offset = (SCREEN_HEIGHT - scaled_height) // 2

    pygame.draw.line(DISPLAY, WHITE,
                     (horizontal_offset, vertical_offset), (horizontal_offset + scaled_width, vertical_offset), 2)
    pygame.draw.line(DISPLAY, WHITE,
                     (horizontal_offset, vertical_offset), (horizontal_offset, vertical_offset + scaled_height), 2)
    pygame.draw.line(DISPLAY, WHITE,
                     (horizontal_offset, vertical_offset + scaled_height), (horizontal_offset + scaled_width, vertical_offset + scaled_height), 2)
    pygame.draw.line(DISPLAY, WHITE,
                     (horizontal_offset + scaled_width, vertical_offset),
                     (horizontal_offset + scaled_width, vertical_offset + scaled_height), 2)

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()

        pygame.display.update()


if __name__ == '__main__':
    main()
