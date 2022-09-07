# Week 1

## Summary

These first few days, I have:

- Set up a Why3 environment.
- Read sections 1-3,5 of the documentation.
- Followed along the 5 examples shown in section 3.
- Attempted to familiarize myself with the Why3 IDE.

## Goals

The first goal is to become familiar with different input methods for Why3.
With that in mind, the primary tasks in the first week are:

- Go through sections 4,6 to become familiar with the intended workflow and IDE.
- Read section 9.
- Write some proofs of my own using various input methods.

The focus will mostly be on WhyML and MLCFG.

## Questions

Q: Another input method/way to interact?
A: MLCFG, microC, etc. are implemented as plugins that transpile to an internal Why3 AST.
   You could do this for a While-language,
   but you have to justify it in regards to the learning objectives.

Why3-internal vs MLCFG:
- Why3 is more direct.
- Why3 is more complex, and not well-documented.
    - Note: A good contribution is to write some tutorial-style documentation
      for making Why3 plugins.
- MLCFG is simpler.
- MLCFG will not have interactive editing.

Adjustments:
- Look into the internal language.
- WhyML is sort-of the external version of the internal model.

## Meeting

Plan: Have an MVP at the end of week 4.
MVP:
- A very small While-language.
- Tooling/integration for proving properties of While-programs in Why3.
- Examples and tests for the tooling.
- Write-up:
    - Description of architecture and __design decisions__, in a loose report-style.
    - Lessons learned, evaluation:
        - Threats to validity?
        - Practical considerations?
    - Hvad har jeg lært, hvad har jeg erfaret, hvad har udbyttet været, hvilke bidrag kommer projektet med?

Additional tasks:
- Read/skim through references in scratchpad, do small write-ups for them.
  No need to go super in-depth.

Notes:

- CVC4 is a good solver.
- Suggestion: Make a reference file,
  where you move over resources once you've read them.
