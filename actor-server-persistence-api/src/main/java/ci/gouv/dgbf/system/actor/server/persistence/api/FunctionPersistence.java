package ci.gouv.dgbf.system.actor.server.persistence.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

import org.cyk.utility.__kernel__.computation.ComparisonOperator;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.server.persistence.PersistenceEntity;

public interface FunctionPersistence extends PersistenceEntity<Function> {

	static Integer computeNumberOfActor(Boolean shared) {
		return Boolean.TRUE.equals(shared) ? 0 : 1;
	}
	
	static Boolean computeShared(Integer numberOfActor) {
		return NumberHelper.compare(numberOfActor, 1, ComparisonOperator.NEQ);
	}
	
	static String computeSharedAsString(Boolean shared) {
		return Boolean.TRUE.equals(shared) ? "Oui" : "Non";
	}
	
	static String computeSharedAsString(Integer numberOfActor) {
		return computeSharedAsString(computeShared(numberOfActor));
	}
}