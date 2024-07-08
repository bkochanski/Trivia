
# Oznaczenia hipotez (opcjonalnie)
hipotezy = ["Choroba", "Brak choroby", "H3", "H4"]

# Zaczątek (rozkład a priori, ang. prior)
prior = [.001, .999-0.1, .1-0.01, 0.01]

# Zdarzalność (prawdopodobieństwa warunkowe danych, ang. likelihood)
likelihood = [.95, .01, .5, .25]

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


x = np.arange(len(hipotezy)) 
width = 0.375

fig, ax = plt.subplots(layout='constrained')

rects = ax.bar(x-width/2, np.round(prior, 4), width, label = 'zaczątek')
ax.bar_label(rects, padding=3)

rects = ax.bar(x+width/2, np.round(posterior, 4), width, label = 'wynik')
ax.bar_label(rects, padding=3)
ax.set_ylabel('prawdopodobieństwo')
ax.set_xticks(x, hipotezy)
ax.legend(loc='upper left', ncols=2)
ax.set_ylim(0, np.max([prior, posterior])*1.2)

plt.show()
