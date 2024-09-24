using System;
using System.Threading;
using System.Text;
using Microsoft.Xna.Framework.Graphics;
using System.Collections.Generic;
using Microsoft.Xna.Framework.Content;
using System.Threading.Tasks;
using System.IO;
using System.Linq;
using System.Text.Json.Serialization;
using Microsoft.Xna.Framework;
using Nez.ParticleDesigner;
using Nez.Sprites;
using Nez.Textures;
using Nez.Tiled;
using Microsoft.Xna.Framework.Audio;
using Microsoft.Xna.Framework.Media;
using Newtonsoft.Json;
using Nez.BitmapFonts;
using Nez.Aseprite;


namespace Nez.Systems
{
	/// <summary>
	/// ContentManager subclass that also manages Effects from ogl files. Adds asynchronous loading of assets as well.
	/// </summary>
	public class NezContentManager : ContentManager
	{
		Dictionary<string, Effect> _loadedEffects = new Dictionary<string, Effect>();

		List<IDisposable> _disposableAssets;

		List<IDisposable> DisposableAssets
		{
			get
			{
				if (_disposableAssets == null)
				{
					var fieldInfo = ReflectionUtils.GetFieldInfo(typeof(ContentManager), "disposableAssets");
					_disposableAssets = fieldInfo.GetValue(this) as List<IDisposable>;
				}

				return _disposableAssets;
			}
		}

#if FNA
		Dictionary<string, object> _loadedAssets;
		Dictionary<string, object> LoadedAssets
		{
			get
			{
				if (_loadedAssets == null)
				{
					var fieldInfo = ReflectionUtils.GetFieldInfo(typeof(ContentManager), "loadedAssets");
					_loadedAssets = fieldInfo.GetValue(this) as Dictionary<string, object>;
				}
				return _loadedAssets;
			}
		}
#endif


		public NezContentManager(IServiceProvider serviceProvider, string rootDirectory) : base(serviceProvider, rootDirectory)
		{}

		public NezContentManager(IServiceProvider serviceProvider) : base(serviceProvider)
		{}

		public NezContentManager() : base(((Game)Core._instance).Services, ((Game)Core._instance).Content.RootDirectory)
		{}

		#region Strongly Typed Loaders
		public Sprite LoadSpriteFromAtlas(string spriteName, string atlasName)
		{
			var spriteTexture = LoadTexture(atlasName);
			var atlasJson = atlasName.Replace(".png", ".json");
			var json = File.ReadAllText(atlasJson);
			
			var lines = json.Split('\n');
			string line = lines.FirstOrDefault(l => l.Contains(spriteName));

			Rectangle rectangle;
			Vector2 origin = default; // pivot
			bool hasOrigin = false;
			if (line != null)
			{
				// Example line:
				// { "name": "TestDrone", "color": "#0000ffff", "keys": [{ "frame": 0, "bounds": {"x": 0, "y": 0, "w": 32, "h": 16 } }] }

				// Find the part that contains the bounds.
				if(!line.Contains("bounds"))
					throw new Exception("sprite bounds not found in atlas");
				
				var boundsPart = line.Substring(line.IndexOf("\"bounds\""));

				// Now extract the numbers from the bounds part.
				int x = ExtractValue(boundsPart, "\"x\":");
				int y = ExtractValue(boundsPart, "\"y\":");
				int width = ExtractValue(boundsPart, "\"w\":");
				int height = ExtractValue(boundsPart, "\"h\":");

				// Create the Rectangle from the extracted values.
				rectangle = new Rectangle(x, y, width, height);
				
				// pivot example:
				// { "name": "DoorEntity", "color": "#0000ffff", "keys": [{ "frame": 0, "bounds": {"x": 32, "y": 0, "w": 16, "h": 48 }, "pivot": {"x": 0, "y": 24 } }] }
				if (line.Contains("pivot"))
				{
					hasOrigin = true;
					var pivotPart = line.Substring(line.IndexOf("\"pivot\""));
					int pivotX = ExtractValue(pivotPart, "\"x\":");
					int pivotY = ExtractValue(pivotPart, "\"y\":");
					origin = new Vector2(pivotX, pivotY);
				}
			}
			else
			{
				Debug.Error("Sprite not found in atlas");
				rectangle = new Rectangle(0, 0, 32, 32);
			}
			
			return !hasOrigin ? new Sprite(spriteTexture, rectangle) : new Sprite(spriteTexture, rectangle, origin);
		}
		
		public SpriteAnimation LoadSpriteAnimationFromSlices(string spriteName, string contentSpritesAssemblyAssPng)
		{
			List<Sprite> sprites = new List<Sprite>();
			List<float> frameRates = new List<float>();

			// Load the texture and JSON data
			var spriteTexture = LoadTexture(contentSpritesAssemblyAssPng);
			var atlasJson = contentSpritesAssemblyAssPng.Replace(".png", ".json");
			var json = File.ReadAllText(atlasJson);

			// Parse the JSON content
			var jsonObject = JsonConvert.DeserializeObject<Dictionary<string, dynamic>>(json);
			var frames = jsonObject["frames"];
			var slices = jsonObject["meta"]["slices"];

			// Find the specific slice by name
			foreach (var slice in slices)
			{
				if (slice["name"].ToString() == spriteName)
				{
					var sliceKeys = slice["keys"];
					foreach (var key in sliceKeys)
					{
						var frameIndex = key["frame"];
						var frameData = frames[$"{frameIndex}"];

						var spriteSourceSize = frameData["spriteSourceSize"];
						var frameBounds = frameData["frame"];
						var bounds = key["bounds"];
						
						Rectangle sourceRect = new Rectangle(
							(int)frameBounds["x"] + (int)bounds["x"] - (int)spriteSourceSize["x"], 
							(int)frameBounds["y"] + (int)bounds["y"] - (int)spriteSourceSize["y"], 
							(int)bounds["w"], 
							(int)bounds["h"]
						);

						// Extract duration for this frame
						float duration = (float)frameData["duration"];
						
						bool hasPivot = key.ContainsKey("pivot");
						Vector2 pivot = hasPivot ? new Vector2((float)key["pivot"]["x"], (float)key["pivot"]["y"]) : Vector2.Zero;

						// Add the sprite and frame rate
						if(!hasPivot)
							sprites.Add(new Sprite(spriteTexture, sourceRect));
						else
							sprites.Add(new Sprite(spriteTexture, sourceRect, pivot));
						frameRates.Add(duration);
					}
					break; // Stop once the spriteName slice is processed
				}
			}

			return new SpriteAnimation(sprites.ToArray(), frameRates.ToArray());
		}

		
		public SpriteAnimation LoadSpriteAnimationFromTags(string spriteName, string atlasName)
		{
			// sprite example name: homing, laser... etc.
			List<Sprite> sprites = [];
			List<float> frameRates = [];
			
			var spriteTexture = LoadTexture(atlasName);
			var atlasJson = atlasName.Replace(".png", ".json");
			var json = File.ReadAllText(atlasJson);
			
			// parse sprites and frame rates from the atlas
			var lines = json.Split('\n');
			Rectangle rectangle;
			for (int i = 0; i < lines.Length; i++)
			{
				if (lines[i].Contains(spriteName))
				{
					if (lines[i+1].Contains("frame"))
					{
						var frameLine = lines[i+1];
						rectangle.X = ExtractValue(frameLine, "\"x\":");
						rectangle.Y = ExtractValue(frameLine, "\"y\":");
						rectangle.Width = ExtractValue(frameLine, "\"w\":");
						rectangle.Height= ExtractValue(frameLine, "\"h\":");
						
						float duration = ExtractValue(lines[i+6], "\"duration\":");
						
						sprites.Add(new Sprite(spriteTexture, rectangle));
						frameRates.Add(duration);
					}
				}
			}
			
			return new SpriteAnimation(sprites.ToArray(), frameRates.ToArray());
		}
		
		public Song LoadSong(string name)
		{
			name = name.Replace(@"Content\", "").Replace(".ogg", "");

			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is Song song1)
				{
					return song1;
				}
			}

			var song = Load<Song>(name);
			LoadedAssets[name] = song;
			DisposableAssets.Add(song);
			return song;
		}
		
		private int ExtractValue(string source, string key)
		{
			// Find the index of the key and extract the value.
			int startIndex = source.IndexOf(key) + key.Length;
			int endIndex = source.IndexOfAny([',', '}'], startIndex); // Find the end of the value.
			if(endIndex == -1)
				endIndex = source.Length;
			string valueString = source.Substring(startIndex, endIndex - startIndex).Trim();
    
			// Convert the extracted string to an integer and return it.
			return int.Parse(valueString);
		}
		
		/// <summary>
		/// loads a Texture2D either from xnb or directly from a png/jpg. Note that xnb files should not contain the .xnb file
		/// extension or be preceded by "Content" in the path. png/jpg files should have the file extension and have an absolute
		/// path or a path starting with "Content".
		/// </summary>
		public Texture2D LoadTexture(string name, bool premultiplyAlpha = false)
		{
			// no file extension. Assumed to be an xnb so let ContentManager load it
			if (string.IsNullOrEmpty(Path.GetExtension(name)))
				return Load<Texture2D>(name);

			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is Texture2D tex)
					return tex;
			}

			using (var stream = Path.IsPathRooted(name) ? File.OpenRead(name) : TitleContainer.OpenStream(name))
			{
				var texture = premultiplyAlpha ? TextureUtils.TextureFromStreamPreMultiplied(stream) : Texture2D.FromStream(Core.GraphicsDevice, stream);
				texture.Name = name;
				LoadedAssets[name] = texture;
				DisposableAssets.Add(texture);

				return texture;
			}
		}

		/// <summary>
		/// loads a SoundEffect either from xnb or directly from a wav. Note that xnb files should not contain the .xnb file
		/// extension or be preceded by "Content" in the path. wav files should have the file extension and have an absolute
		/// path or a path starting with "Content".
		/// </summary>
		public SoundEffect LoadSoundEffect(string name)
		{
			// no file extension. Assumed to be an xnb so let ContentManager load it
			if (string.IsNullOrEmpty(Path.GetExtension(name)))
				return Load<SoundEffect>(name);

			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is SoundEffect sfx)
				{
					return sfx;
				}
			}

			using (var stream = Path.IsPathRooted(name) ? File.OpenRead(name) : TitleContainer.OpenStream(name))
			{
				var sfx = SoundEffect.FromStream(stream);
				LoadedAssets[name] = sfx;
				DisposableAssets.Add(sfx);
				return sfx;
			}
		}

		/// <summary>
		/// loads a Tiled map
		/// </summary>
		public TmxMap LoadTiledMap(string name)
		{
			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is TmxMap map)
					return map;
			}

			var tiledMap = new TmxMap().LoadTmxMap(name, this);

			LoadedAssets[name] = tiledMap;
			DisposableAssets.Add(tiledMap);

			return tiledMap;
		}

		/// <summary>
		/// Loads a ParticleDesigner pex file
		/// </summary>
		public Particles.ParticleEmitterConfig LoadParticleEmitterConfig(string name)
		{
			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is Particles.ParticleEmitterConfig config)
					return config;
			}

			var emitterConfig = ParticleEmitterConfigLoader.Load(name);

			LoadedAssets[name] = emitterConfig;
			DisposableAssets.Add(emitterConfig);

			return emitterConfig;
		}

		/// <summary>
		/// Loads a SpriteAtlas created with the Sprite Atlas Packer tool
		/// </summary>
		public SpriteAtlas LoadSpriteAtlas(string name, bool premultiplyAlpha = false)
		{
			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is SpriteAtlas spriteAtlas)
					return spriteAtlas;
			}

			var atlas = SpriteAtlasLoader.ParseSpriteAtlas(name, premultiplyAlpha);

			LoadedAssets.Add(name, atlas);
			DisposableAssets.Add(atlas);

			return atlas;
		}

		/// <summary>
		/// Loads a BitmapFont
		/// </summary>
		public BitmapFont LoadBitmapFont(string name, bool premultiplyAlpha = false)
		{
			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is BitmapFont bmFont)
					return bmFont;
			}

			var font = BitmapFontLoader.LoadFontFromFile(name, premultiplyAlpha);

			LoadedAssets.Add(name, font);
			DisposableAssets.Add(font);

			return font;
		}

		/// <summary>
		/// Loads the contents of an Aseprite (.ase/.aseprite) file.
		/// </summary>
		/// <param name="name">The content path name of the Aseprite file to load.</param>
		/// <returns>
		/// A new instance of the <see cref="AsepriteFile"/> class initialized with the data read from the Aseprite
		/// file.
		/// </returns>
		public AsepriteFile LoadAsepriteFile(string name)
		{
			if (LoadedAssets.TryGetValue(name, out var asset))
			{
				if (asset is AsepriteFile aseFile)
					return aseFile;
			}

			var asepriteFile = AsepriteFileLoader.Load(name);
			LoadedAssets.Add(name, asepriteFile);
			return asepriteFile;
		}

		/// <summary>
		/// loads an ogl effect directly from file and handles disposing of it when the ContentManager is disposed. Name should be the path
		/// relative to the Content folder or including the Content folder.
		/// </summary>
		/// <returns>The effect.</returns>
		/// <param name="name">Name.</param>
		public Effect LoadEffect(string name) => LoadEffect<Effect>(name);

		/// <summary>
		/// loads an embedded Nez effect. These are any of the Effect subclasses in the Nez/Graphics/Effects folder.
		/// Note that this will return a unique instance if you attempt to load the same Effect twice to avoid Effect duplication.
		/// </summary>
		/// <returns>The nez effect.</returns>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public T LoadNezEffect<T>() where T : Effect, new()
		{
			var cacheKey = typeof(T).Name + "-" + Utils.RandomString(5);
			var effect = new T();
			effect.Name = cacheKey;
			_loadedEffects[cacheKey] = effect;

			return effect;
		}

		/// <summary>
		/// loads an ogl effect directly from file and handles disposing of it when the ContentManager is disposed. Name should the the path
		/// relative to the Content folder or including the Content folder. Effects must have a constructor that accepts GraphicsDevice and
		/// byte[]. Note that this will return a unique instance if you attempt to load the same Effect twice to avoid Effect duplication.
		/// </summary>
		/// <returns>The effect.</returns>
		/// <param name="name">Name.</param>
		public T LoadEffect<T>(string name) where T : Effect
		{
			// make sure the effect has the proper root directory
			if (!name.StartsWith(RootDirectory))
				name = RootDirectory + "/" + name;

			var bytes = EffectResource.GetFileResourceBytes(name);

			return LoadEffect<T>(name, bytes);
		}

		/// <summary>
		/// loads an ogl effect directly from its bytes and handles disposing of it when the ContentManager is disposed. Name should the the path
		/// relative to the Content folder or including the Content folder. Effects must have a constructor that accepts GraphicsDevice and
		/// byte[]. Note that this will return a unique instance if you attempt to load the same Effect twice to avoid Effect duplication.
		/// </summary>
		/// <returns>The effect.</returns>
		/// <param name="name">Name.</param>
		public T LoadEffect<T>(string name, byte[] effectCode) where T : Effect
		{
			var effect = Activator.CreateInstance(typeof(T), Core.GraphicsDevice, effectCode) as T;
			effect.Name = name + "-" + Utils.RandomString(5);
			_loadedEffects[effect.Name] = effect;

			return effect;
		}

		/// <summary>
		/// loads and manages any Effect that is built-in to MonoGame such as BasicEffect, AlphaTestEffect, etc. Note that this will
		/// return a unique instance if you attempt to load the same Effect twice. If you intend to use the same Effect in multiple locations
		/// keep a reference to it and use it directly.
		/// </summary>
		/// <returns>The mono game effect.</returns>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public T LoadMonoGameEffect<T>() where T : Effect
		{
			var effect = Activator.CreateInstance(typeof(T), Core.GraphicsDevice) as T;
			effect.Name = typeof(T).Name + "-" + Utils.RandomString(5);
			_loadedEffects[effect.Name] = effect;

			return effect;
		}

		#endregion

		/// <summary>
		/// loads an asset on a background thread with optional callback for when it is loaded. The callback will occur on the main thread.
		/// </summary>
		/// <param name="assetName">Asset name.</param>
		/// <param name="onLoaded">On loaded.</param>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public void LoadAsync<T>(string assetName, Action<T> onLoaded = null)
		{
			var syncContext = SynchronizationContext.Current;
			Task.Run(() =>
			{
				var asset = Load<T>(assetName);

				// if we have a callback do it on the main thread
				if (onLoaded != null)
				{
					syncContext.Post(d => { onLoaded(asset); }, null);
				}
			});
		}

		/// <summary>
		/// loads an asset on a background thread with optional callback that includes a context parameter for when it is loaded.
		/// The callback will occur on the main thread.
		/// </summary>
		/// <param name="assetName">Asset name.</param>
		/// <param name="onLoaded">On loaded.</param>
		/// <param name="context">Context.</param>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public void LoadAsync<T>(string assetName, Action<object, T> onLoaded = null, object context = null)
		{
			var syncContext = SynchronizationContext.Current;
			Task.Run(() =>
			{
				var asset = Load<T>(assetName);

				if (onLoaded != null)
				{
					syncContext.Post(d => { onLoaded(context, asset); }, null);
				}
			});
		}

		/// <summary>
		/// loads a group of assets on a background thread with optional callback for when it is loaded
		/// </summary>
		/// <param name="assetNames">Asset names.</param>
		/// <param name="onLoaded">On loaded.</param>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public void LoadAsync<T>(string[] assetNames, Action onLoaded = null)
		{
			var syncContext = SynchronizationContext.Current;
			Task.Run(() =>
			{
				for (var i = 0; i < assetNames.Length; i++)
					Load<T>(assetNames[i]);

				// if we have a callback do it on the main thread
				if (onLoaded != null)
				{
					syncContext.Post(d => { onLoaded(); }, null);
				}
			});
		}

		/// <summary>
		/// removes assetName from LoadedAssets and Disposes of it
		/// disposeableAssets List.
		/// </summary>
		/// <param name="assetName">Asset name.</param>
		/// <typeparam name="T">The 1st type parameter.</typeparam>
		public void UnloadAsset<T>(string assetName) where T : class, IDisposable
		{
			if (IsAssetLoaded(assetName))
			{
				try
				{
					// first fetch the actual asset. we already know its loaded so we'll grab it directly
					var assetToRemove = LoadedAssets[assetName];
					for (var i = 0; i < DisposableAssets.Count; i++)
					{
						// see if the asset is disposeable. If so, find and dispose of it.
						var typedAsset = DisposableAssets[i] as T;
						if (typedAsset != null && typedAsset == assetToRemove)
						{
							typedAsset.Dispose();
							DisposableAssets.RemoveAt(i);
							break;
						}
					}

					LoadedAssets.Remove(assetName);
				}
				catch (Exception e)
				{
					Debug.Error("Could not unload asset {0}. {1}", assetName, e);
				}
			}
		}

		/// <summary>
		/// unloads an Effect that was loaded via loadEffect, loadNezEffect or loadMonoGameEffect
		/// </summary>
		/// <param name="effectName">Effect.name</param>
		public bool UnloadEffect(string effectName)
		{
			if (_loadedEffects.ContainsKey(effectName))
			{
				_loadedEffects[effectName].Dispose();
				_loadedEffects.Remove(effectName);
				return true;
			}

			return false;
		}

		/// <summary>
		/// unloads an Effect that was loaded via loadEffect, loadNezEffect or loadMonoGameEffect
		/// </summary>
		public bool UnloadEffect(Effect effect) => UnloadEffect(effect.Name);

		/// <summary>
		/// checks to see if an asset with assetName is loaded
		/// </summary>
		/// <returns><c>true</c> if this instance is asset loaded the specified assetName; otherwise, <c>false</c>.</returns>
		/// <param name="assetName">Asset name.</param>
		public bool IsAssetLoaded(string assetName) => LoadedAssets.ContainsKey(assetName);

		/// <summary>
		/// provides a string suitable for logging with all the currently loaded assets and effects
		/// </summary>
		/// <returns>The loaded assets.</returns>
		internal string LogLoadedAssets()
		{
			var builder = new StringBuilder();
			foreach (var asset in LoadedAssets.Keys)
				builder.AppendFormat("{0}: ({1})\n", asset, LoadedAssets[asset].GetType().Name);

			foreach (var asset in _loadedEffects.Keys)
				builder.AppendFormat("{0}: ({1})\n", asset, _loadedEffects[asset].GetType().Name);

			return builder.ToString();
		}

		/// <summary>
		/// reverse lookup. Gets the asset path given the asset. This is useful for making editor and non-runtime stuff.
		/// </summary>
		/// <param name="asset"></param>
		/// <returns></returns>
		public string GetPathForLoadedAsset(object asset)
		{
			if (LoadedAssets.ContainsValue(asset))
			{
				foreach (var kv in LoadedAssets)
				{
					if (kv.Value == asset)
						return kv.Key;
				}
			}

			return null;
		}

		/// <summary>
		/// override that disposes of all loaded Effects
		/// </summary>
		public override void Unload()
		{
			base.Unload();

			foreach (var key in _loadedEffects.Keys)
				_loadedEffects[key].Dispose();

			_loadedEffects.Clear();
		}

		
	}


	/// <summary>
	/// the only difference between this class and NezContentManager is that this one can load embedded resources from the Nez.dll
	/// </summary>
	sealed class NezGlobalContentManager : NezContentManager
	{
		public NezGlobalContentManager(IServiceProvider serviceProvider, string rootDirectory) : base(serviceProvider, rootDirectory)
		{}

		/// <summary>
		/// override that will load embedded resources if they have the "nez://" prefix
		/// </summary>
		/// <returns>The stream.</returns>
		/// <param name="assetName">Asset name.</param>
		protected override Stream OpenStream(string assetName)
		{
			if (assetName.StartsWith("nez://"))
			{
				var assembly = GetType().Assembly;

#if FNA
				// for FNA, we will just search for the file by name since the assembly name will not be known at runtime
				foreach (var item in assembly.GetManifestResourceNames())
				{
					if (item.EndsWith(assetName.Substring(assetName.Length - 20)))
					{
						assetName = "nez://" + item;
						break;
					}
				}
#endif
				return assembly.GetManifestResourceStream(assetName.Substring(6));
			}

			return base.OpenStream(assetName);
		}
	}
}