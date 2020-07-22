// Cian Steenkamp
package cilib.research.mgpso

// not purely functional due to vars but I value working code over time wasting; good enough is good enough sometimes.
// at iteration t contains ratio of knee points to non-dominated solutions at iteration t − 1
// updated with each MGPSO.calcVelocity
class R_prevs {
	// a private field can be seen by any instance of this class
	// ratio of knee points to non-dominated solutions at iteration t − 1
	private var prev_ratio_KPs_2_ND_sols: Double = 0.0 // init 0.0
	// ratio of the neighbourhood size to the range spanned by objective m at iteration t - 1
	private var prev_ratio: Double = 1.0 // init 1.0
	// setters
	def set_prev_ratio_KPs_2_ND_sols(newPrev: Double) { prev_ratio_KPs_2_ND_sols = newPrev }
	def set_prev_ratio(newPrev: Double) { prev_ratio = newPrev }
	// getters
	def get_prev_ratio_KPs_2_ND_sols: Double = prev_ratio_KPs_2_ND_sols
	def get_prev_ratio: Double = prev_ratio
}
