import pandas as pd
import matplotlib.pyplot as plt

df = pd.DataFrame({
    'Hipotezy': pd.Categorical(['Choroba', 'Brak choroby', 'Choroba', 'Brak choroby'], 
                              categories=['Choroba', 'Brak choroby']),
    'Rozkład': pd.Categorical(['zaczątek', 'zaczątek', 'wynik', 'wynik'],
                             categories=['zaczątek', 'wynik']),
    'Prawdopodobieństwo': [0.001, 0.999, 0.087, 0.913]
})

fig, ax = plt.subplots()
df.plot(x='Hipotezy', y='Prawdopodobieństwo', kind='bar', fill=True, ax=ax)
ax.set_xlabel('Hipotezy')
ax.set_ylabel('Prawdopodobieństwo')
ax.legend(title='Rozkład')
for i, v in enumerate(df['Prawdopodobieństwo']):
    ax.text(i, v, f"{v:.4f}", ha='center', va='bottom')
plt.show()

import matplotlib.pyplot as plt
import numpy as np

species = ("Adelie", "Chinstrap", "Gentoo")
penguin_means = {
    'Bill Depth': (18.35, 18.43, 14.98),
    'Bill Length': (38.79, 48.83, 47.50),
    'Flipper Length': (189.95, 195.82, 217.19),
}

x = np.arange(len(species))  # the label locations
width = 0.25  # the width of the bars
multiplier = 0

fig, ax = plt.subplots(layout='constrained')

for attribute, measurement in penguin_means.items():
    offset = width * multiplier
    rects = ax.bar(x + offset, measurement, width, label=attribute)
    ax.bar_label(rects, padding=3)
    multiplier += 1

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Length (mm)')
ax.set_title('Penguin attributes by species')
ax.set_xticks(x + width, species)
ax.legend(loc='upper left', ncols=3)
ax.set_ylim(0, 250)

plt.show()

# Oznaczenia hipotez (opcjonalnie)
hipotezy = ["Choroba", "Brak choroby"]

# Zaczątek (rozkład a priori, ang. prior)
prior = [.001, .999]

# Zdarzalność (prawdopodobieństwa warunkowe danych, ang. likelihood)
likelihood = [.95, .01]

# Wyznaczenie wyniku (rozkładu a posteriori, ang. posterior)
posterior = [a*b for a, b in zip(prior, likelihood)]
posterior = [p/sum(posterior) for p in posterior]

# Sprawdzenie
if len(prior) != len(likelihood):
    print("Liczebność wektorów prior (zaczątek) i likelihood (zdarzalność) powinna być równa.")
if sum(prior) != 1:
    print("Suma prawdopodobieństw w rozkładzie zaczątkowym (prior) powinna być równa 1.")
if not "hipotezy" in locals() or len(hipotezy) != len(prior):
    hipotezy = ["H" + str(i) for i in range(1, len(prior)+1)]

# Wynik w formie ramki danych:
import pandas as pd
df = pd.DataFrame({
    "Hipotezy": hipotezy,
    "Zaczątek": prior,
    "Zdarzalność": likelihood,
    "Wynik": posterior
})
print(df)


x = np.arange(len(hipotezy))  # the label locations
width = (1-0.25)/len(hipotezy)   # the width of the bars
multiplier = 0

fig, ax = plt.subplots(layout='constrained')

offset = width * multiplier
rects = ax.bar(x+ offset, np.round(prior, 4), width, label = 'zaczątek')
ax.bar_label(rects, padding=3)

multiplier = 1

offset = width * multiplier
rects = ax.bar(x + offset, np.round(posterior, 4), width, label = 'wynik')
ax.bar_label(rects, padding=3)

ax.set_ylabel('prawdopodobieństwo')
ax.set_title('wzór Bayesa')
ax.set_xticks(x+width/2, hipotezy)
ax.legend(loc='upper left', ncols=2)

plt.show()


for attribute, measurement in penguin_means.items():
    offset = width * multiplier
    rects = ax.bar(x + offset, measurement, width, label=attribute)
    ax.bar_label(rects, padding=3)
    multiplier += 1
