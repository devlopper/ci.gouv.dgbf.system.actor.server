package ci.gouv.dgbf.system.actor.server.persistence.api;

import org.cyk.utility.server.persistence.PersistenceEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionPersistence extends PersistenceEntity<ScopeFunction> {

	static Integer computeNumberOfActor(Boolean shared) {
		return FunctionPersistence.computeNumberOfActor(shared);
	}
	
	static Boolean computeShared(Integer numberOfActor) {
		return FunctionPersistence.computeShared(numberOfActor);
	}
	
	static String computeSharedAsString(Boolean shared) {
		return FunctionPersistence.computeSharedAsString(shared);
	}
}