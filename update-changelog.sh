#!/bin/bash

# Genera el changelog y lo guarda en CHANGELOG.md
git cliff > CHANGELOG.md

echo "CHANGELOG actualizado exitosamente."
