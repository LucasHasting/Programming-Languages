happy(taylor).
listensToMusic(mary).
coding(taylor) :- listensToMusic(taylor), happy(taylor).
coding(mary) :- happy(mary).
coding(mary) :- listensToMusic(mary).