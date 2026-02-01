sync-m3:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/_targets/ ./_targets/

sync-m3-edfs:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/edfs/ ./edfs/

sync-m3-xml:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/edfs/*.XML ./edfs/

push-m3-edfs:
    rsync -avrz --progress /s/Epi-Dementia/Pase-ED/Studies/BACH_Sleep/edfs/Displayedsignals_inclFiltering/ m3:bc41_scratch2/Spindles/edfs/

push-m3:
    rsync -avrz --progress ./_targets/ m3:bc41_scratch2/Spindles/_targets/
