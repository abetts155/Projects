import pygame
import time
from pygame.locals import (RLEACCEL, K_UP, K_DOWN, K_LEFT, K_RIGHT, K_ESCAPE, K_SPACE, KEYDOWN, QUIT)


WIDTH = 1280
HEIGHT = 720


class Budgie(pygame.sprite.Sprite):
    def __init__(self):
        super(Budgie, self).__init__()
        self.surf = pygame.transform.scale(surface=pygame.image.load("charlie2.png").convert_alpha(),
                                           size=(180, 180))
        self.surf.set_colorkey((255, 255, 255), RLEACCEL)
        self.rect = self.surf.get_rect()
        self.rect.x = 0
        self.rect.y = HEIGHT

    def update(self, pressed_keys, screen_width, screen_height):
        if pressed_keys[K_UP]:
            self.rect.move_ip(0, -5)
        if pressed_keys[K_DOWN]:
            self.rect.move_ip(0, 5)
        if pressed_keys[K_LEFT]:
            self.rect.move_ip(-5, 0)
        if pressed_keys[K_RIGHT]:
            self.rect.move_ip(5, 0)
        if pressed_keys[K_SPACE]:
            print('YEP')

        # Keep player on the screen
        if self.rect.left < 0:
            self.rect.left = 0
        if self.rect.right > screen_width:
            self.rect.right = screen_width
        if self.rect.top <= 0:
            self.rect.top = 0
        if self.rect.bottom >= screen_height:
            self.rect.bottom = screen_height


def main():
    main_surface = pygame.display.set_mode([WIDTH, HEIGHT])
    screen_width, screen_height = main_surface.get_size()
    pygame.display.set_caption('Super Charlie!')
    background = pygame.image.load("background.png")
    background = pygame.transform.scale(background, (screen_width, screen_height))
    clock = pygame.time.Clock()
    example_sound = pygame.mixer.Sound('sound.wav')

    charlie = Budgie()

    # Run until the user asks to quit
    running = True
    while running:
        main_surface.blit(background, (0, 0))

        # Did the user click the window close button?
        for event in pygame.event.get():
            if event.type == KEYDOWN:
                if event.key == K_ESCAPE:
                    running = False

                if event.key == K_UP:
                    example_sound.play()
                    time.sleep(0.5)
                    example_sound.stop()

            if event.type == pygame.QUIT:
                running = False

        pressed_keys = pygame.key.get_pressed()
        charlie.update(pressed_keys, screen_width, screen_height)
        main_surface.blit(charlie.surf, charlie.rect)

        pygame.display.update()
        #clock.tick(60)


if __name__ == '__main__':
    pygame.init()
    main()
    pygame.quit()
