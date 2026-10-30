#!/usr/bin/env bash

# export LD_LIBRARY_PATH="$HOME/voicevox_core/0.17.0/download/voicevox_core/onnxruntime/lib:$LD_LIBRARY_PATH"
./example/vv-wrapper-example.ros --text "Test" \
--lib-core-path /home/user/voicevox/core_data/0.17.1/voicevox_core/c_api/lib/libvoicevox_core.so \
--lib-onnxruntime-path /home/user/voicevox/core_data//0.17.1/voicevox_core/onnxruntime/lib/libvoicevox_onnxruntime.so.1.17.3  \
--open-jtalk-dict-dir-path ~/voicevox/core_data/0.17.1/voicevox_core/dict/open_jtalk_dic_utf_8-1.11 \
--vvm-path /home/user/voicevox/core_data/0.17.0/voicevox_core/models/vvms/0.vvm \
--speaker-id 1 \
--output-file ./test.wav
