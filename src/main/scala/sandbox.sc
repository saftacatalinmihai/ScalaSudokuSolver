val chars = "23857\n495".replace("\n","").split("""|\n""")
val slices = chars.sliding(6)
for (s <- slices) println(s(0))