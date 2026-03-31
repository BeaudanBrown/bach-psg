default:
    @just --list

manifest:
    Rscript -e 'targets::tar_manifest()'

run:
    Rscript -e 'targets::tar_make()'

psd-csv:
    Rscript -e 'targets::tar_make(psd_csv)'

sync-m3:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/_targets/ ./_targets/

sync-m3-edfs:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/edfs/ ./edfs/

sync-m3-xml:
    rsync -avrz --progress --delete m3:bc41_scratch2/Spindles/edfs/*.XML ./edfs/

push-m3-edfs:
    rsync -avruz --progress /s/Pase-ED/Studies/BACH_Sleep/edfs/Displayedsignals_inclFiltering/ m3:bc41_scratch2/Spindles/edfs/

push-m3-xml:
    rsync -avrzI --progress /s/Pase-ED/Studies/BACH_Sleep/edfs/Displayedsignals_inclFiltering/*.XML m3:bc41_scratch2/Spindles/edfs/

push-m3:
    rsync -avrz --progress ./_targets/ m3:bc41_scratch2/Spindles/_targets/

push-rack-edfs:
    rsync -avrz --progress /s/Pase-ED/Studies/BACH_Sleep/edfs/Displayedsignals_inclFiltering/ bottom:documents/Spindles/edfs/
