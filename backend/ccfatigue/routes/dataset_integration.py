from fastapi import APIRouter, UploadFile, File, HTTPException
from pathlib import Path
import shutil
import zipfile

router = APIRouter()

@router.post("/experiments/integrate_dataset")
async def integrate_dataset(file: UploadFile = File(...)):
    # Percorso di destinazione
    extract_path = Path("../Data/raw")
    extract_path.mkdir(parents=True, exist_ok=True)

    # Salva temporaneamente il file ZIP
    temp_zip_path = Path(f"temp_{file.filename}")
    with temp_zip_path.open("wb") as buffer:
        shutil.copyfileobj(file.file, buffer)

    # Estrai il contenuto
    try:
        with zipfile.ZipFile(temp_zip_path, "r") as zip_ref:
            zip_ref.extractall(extract_path)
    except zipfile.BadZipFile:
        raise HTTPException(status_code=400, detail="Il file non è un archivio ZIP valido.")
    finally:
        temp_zip_path.unlink()  # Elimina il file temporaneo

    return {"success": True}
