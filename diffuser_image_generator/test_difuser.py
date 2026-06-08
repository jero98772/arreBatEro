import torch
from diffusers import StableDiffusionXLPipeline, EulerAncestralDiscreteScheduler

# 1. Path to your local safetensors model
model_path = "cyberrealisticPony_v180Coreshift.safetensors"

# 2. Load the pipeline using the single file loader for SDXL
print("Loading model... This might take a minute.")
pipe = StableDiffusionXLPipeline.from_single_file(
    model_path, 
    torch_dtype=torch.float16, 
    use_safetensors=True
)

# --- VRAM OPTIMIZATIONS FOR 8GB CARDS ---
# Instead of pipe.to("cuda"), we use CPU offloading to load pieces into VRAM only when needed
pipe.enable_model_cpu_offload()

# This forces the VAE (where your crash happened) to process in smaller chunks
pipe.enable_vae_slicing()
pipe.enable_vae_tiling()
# ----------------------------------------

# 3. Set the scheduler
pipe.scheduler = EulerAncestralDiscreteScheduler.from_config(pipe.scheduler.config)

# 4. Define prompts (Pony specific tags)
prompt = "Professional healthcare awareness flyer design, blood donation campaign poster, modern medical advertisement, clean white background, large bold red typography, heart-shaped blood bag filled with red liquid, realistic transparent plastic blood bag, vector and photorealistic mixed design, medical infographic style, elegant layout, high visual hierarchy, Spanish promotional flyer, healthcare marketing material, charity campaign poster, minimalistic design, red and white color palette, modern sans-serif typography, informative icons, donation symbols, hearts, clean spacing, premium graphic design, magazine quality, corporate branding style, high resolution, print-ready, A4 vertical format, realistic reflections, glossy blood bag, professional poster composition, persuasive healthcare advertising, NGO campaign, emergency blood donation awareness, visually balanced, sharp details, ultra high quality"
negative_prompt = "low quality, blurry, pixelated, distorted anatomy, ugly design, cluttered layout, messy composition, bad typography, unreadable text, watermark, logo, signature, extra objects, cropped elements, low contrast, dark background, horror style, blood splatter, gore, realistic wounds, violence, medical injury, chaotic design, overlapping text, duplicated icons, broken perspective, amateur graphic design, poor spacing, noisy image, jpeg artifacts, misspelled words, deformed shapes, low resolution, oversaturated colors, random text, extra limbs, distorted heart, malformed blood bag, cartoonish, childish design"

# 5. Generate the image
print("Generating image... (Optimized for low VRAM)")
image = pipe(
    prompt=prompt,
    negative_prompt=negative_prompt,
    num_inference_steps=30,  
    guidance_scale=7.0,      
    width=832,               # Keeping resolution slightly lower than 1024x1024 helps stable memory
    height=1216
).images[0]

# 6. Save the result
image.save("cyberrealistic_output.png")
print("Success! Image saved as cyberrealistic_output.png!")