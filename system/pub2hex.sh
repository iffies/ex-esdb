#! /bin/bash

mix hex.build

mix docs

mix hex.user auth

mix hex.publish
