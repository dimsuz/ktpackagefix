{-# LANGUAGE OverloadedStrings #-}
module ViewBindingMigration where

-- * Find viewLayoutResource, derive binding name
-- * Gather ids from layout file and replace them with binding.id in controller file
-- * Rename ScopedMviControllerOld -> ScopedMviController
-- * Rename import for ScopedMviController
-- * Add binding name to ScopedMviController template params
-- * Add binding name to Config template params
-- * Remove viewLayoutResource, replace with binding inflate reference
-- * Remove imports: synthetics, ".R$"
-- * Add imports: ViewBinding, derived binding name

refactorToViewBinding :: IO ()
refactorToViewBinding = error "todo"
