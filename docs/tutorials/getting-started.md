# Getting Started with RONIN
## Your First RONIN Program

Welcome to the **RONIN language** - a domain-specific language designed for audio processing and music composition in the MAITREYA DAW. This tutorial will guide you through creating your first RONIN program.

## 💚 What is RONIN?

RONIN is a functional programming language specifically designed for:
- **Mathematical Audio Processing**: SIEGE expressions with golden ratio integration
- **Type-Safe Composition**: Strong static typing prevents runtime errors
- **Multi-Target Compilation**: Generate code for Haskell, Rust, and Clojure
- **Educational Excellence**: Learn through mathematical beauty and clear syntax

## 🚀 Installation

### Prerequisites
- **Haskell Stack**: For building the RONIN compiler
- **Git**: For cloning the repository

### Build the Compiler
```bash
# Clone the repository
git clone https://github.com/kae3g/ronin-compiler.git
cd ronin-compiler

# Build with Stack
stack build

# Install globally
stack install
```

### Verify Installation
```bash
ronin --version
# Should output: RONIN Compiler v0.1.0 - MAITREYA DAW Language Compiler
```

## 📝 Your First RONIN Program

Create a file called `hello-synth.ronin`:

```ronin
-- Hello Synthesizer
-- Your first RONIN program

module HelloSynth where

-- Simple sine wave synthesizer
sineSynth :: Frequency -> Amplitude -> AudioSignal
sineSynth freq amp = siege $ sin (const freq * 2π * time) * const amp

-- Add golden ratio harmony
goldenSynth :: Frequency -> AudioSignal
goldenSynth base = sineSynth base 0.8 |> gain (φ * 0.618)

-- Main output
main :: AudioSignal
main = goldenSynth 440

-- Type definitions
type Frequency = Float
type Amplitude = Float
type AudioSignal = Signal Float

-- Helper functions
gain :: Float -> AudioSignal -> AudioSignal
gain amount signal = signal * const amount
```

## 🔨 Compiling Your Program

### Compile to Haskell
```bash
ronin compile --target haskell hello-synth.ronin
# Generates: hello-synth.hs
```

### Compile to Rust
```bash
ronin compile --target rust hello-synth.ronin -o hello-synth.rs
```

### Compile to Clojure
```bash
ronin compile --target clojure hello-synth.ronin -o hello-synth.clj
```

## 🧪 Type Checking

Before compilation, you can check your program for type errors:

```bash
ronin typecheck hello-synth.ronin
# Output: Type checking successful
```

## 🎵 Understanding the Code

Let's break down your first RONIN program:

### Module Declaration
```ronin
module HelloSynth where
```
Every RONIN program starts with a module declaration. This creates a namespace for your code.

### Function Definition
```ronin
sineSynth :: Frequency -> Amplitude -> AudioSignal
sineSynth freq amp = siege $ sin (const freq * 2π * time) * const amp
```

- **Type Signature**: `::` declares the function's type
- **Parameters**: `freq` and `amp` are the input parameters
- **SIEGE Expression**: `siege $` introduces mathematical audio processing
- **Mathematical Functions**: `sin`, `const`, `time` are SIEGE primitives

### Golden Ratio Integration
```ronin
goldenSynth base = sineSynth base 0.8 |> gain (φ * 0.618)
```

- **φ (phi)**: The golden ratio constant (≈ 1.618)
- **Pipeline Operator**: `|>` chains function applications
- **Mathematical Beauty**: Golden ratio creates naturally pleasing proportions

## 🎯 Key Concepts

### SIEGE Expressions
SIEGE (Sample-accurate Integer Execution of Garden Expressions) is RONIN's mathematical audio processing language:

```ronin
-- Basic SIEGE primitives
siege $ sin (440 * 2π * time)           -- Sine wave at 440Hz
siege $ cos (time * φ)                  -- Cosine with golden ratio
siege $ exp (-time * 0.1)               -- Exponential decay
```

### Type System
RONIN has a powerful type system:

```ronin
-- Basic types
type Frequency = Float
type Amplitude = Float
type AudioSignal = Signal Float

-- Function types
type Synthesizer = Frequency -> Amplitude -> AudioSignal
type Filter = AudioSignal -> AudioSignal
type Envelope = Time -> Amplitude
```

### Pattern Matching
RONIN supports pattern matching for elegant code:

```ronin
filter :: FilterType -> AudioSignal -> AudioSignal
filter LowPass signal = lowpass signal
filter HighPass signal = highpass signal
filter BandPass signal = bandpass signal
```

## 🔍 Common Patterns

### ADSR Envelope
```ronin
adsr :: Float -> Float -> Float -> Float -> Envelope
adsr attack decay sustain release = siege $ case time of
  t | t < attack -> t / attack
  t | t < attack + decay -> 1 + (sustain - 1) * (t - attack) / decay
  t | t < attack + decay + release -> sustain * (1 - (t - attack - decay) / release)
  _ -> 0
```

### Complex Synthesis
```ronin
complexSynth :: Frequency -> AudioSignal
complexSynth base = siege $ 
  sin (base * 2π * time) * 
  cos (base * φ * 2π * time) * 
  exp (-time * φ * 0.1)
```

## 🚨 Common Errors

### Type Mismatch
```ronin
-- ❌ Error: Type mismatch
sineSynth 440 "loud"  -- String instead of Float

-- ✅ Correct
sineSynth 440 0.8
```

### Undefined Variable
```ronin
-- ❌ Error: Undefined variable
main = undefinedFunction 440

-- ✅ Correct
main = sineSynth 440 0.8
```

## 📚 Next Steps

Now that you've created your first RONIN program, explore these resources:

1. **[Advanced SIEGE Expressions](advanced-siege.md)** - Deep dive into mathematical audio processing
2. **[Type System Guide](type-system.md)** - Understanding RONIN's powerful type system
3. **[Pattern Matching](pattern-matching.md)** - Elegant control flow with patterns
4. **[Multi-Target Compilation](compilation-targets.md)** - Understanding different output formats
5. **[Mathematical Composition](mathematical-composition.md)** - Creating beautiful mathematical relationships

## 🎵 Example Projects

Try these example projects to deepen your understanding:

- **[Simple Synthesizer](../examples/simple-synth.ronin)** - Basic audio synthesis
- **[Mathematical Composition](../examples/math-composition.ronin)** - Golden ratio and fractal composition
- **[Filter Chains](../examples/filter-chain.ronin)** - Complex audio processing

## 💡 Tips for Success

1. **Start Simple**: Begin with basic sine waves and gradually add complexity
2. **Use Type Annotations**: Explicit types help catch errors early
3. **Embrace Mathematics**: RONIN's power comes from mathematical relationships
4. **Experiment with φ**: The golden ratio creates naturally pleasing sounds
5. **Read Error Messages**: RONIN provides helpful type error messages

## 💚 The Gentle Revolution

RONIN embodies the gentle revolution principles:

- **Mathematical Beauty**: Every construct has mathematical meaning
- **Educational Value**: Code teaches as it executes
- **Type Safety**: Prevents errors at compile time
- **Community**: Learn and grow with others

Welcome to the gentle revolution in audio programming! 💚💙

---

**Next**: [Advanced SIEGE Expressions](advanced-siege.md) | [Back to Documentation](../README.md)
