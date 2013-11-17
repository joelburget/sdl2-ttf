#include "SDL_ttf.h"
module Graphics.UI.SDL.TTF.Version
    ( compiledFor
    , linkedWith
    ) where

import Control.Applicative
import Data.Version (Version(Version))

import Foreign (Word8, Ptr, Storable(sizeOf, alignment, peekByteOff, peek))

data SDLVersion
    = SDLVersion Word8 Word8 Word8

instance Storable SDLVersion where
    sizeOf _ = #{size SDL_version}
    alignment _ = 1
    peek ptr = SDLVersion
        <$> #{peek SDL_version, major} ptr
        <*> #{peek SDL_version, minor} ptr
        <*> #{peek SDL_version, patch} ptr

compiledFor :: Version
compiledFor = Version [ #{const TTF_MAJOR_VERSION}
                      , #{const TTF_MINOR_VERSION}
                      , #{const TTF_PATCHLEVEL}
                      ] []

foreign import ccall unsafe "TTF_Linked_Version" sdlLinkedVersion :: IO (Ptr SDLVersion)
linkedWith :: IO Version
linkedWith = do versionPtr <- sdlLinkedVersion
                SDLVersion major minor patch <- peek versionPtr
                return (Version (map fromIntegral [major,minor,patch]) [])
