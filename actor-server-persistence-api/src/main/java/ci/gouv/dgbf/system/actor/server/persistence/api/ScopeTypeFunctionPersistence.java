package ci.gouv.dgbf.system.actor.server.persistence.api;

import org.cyk.utility.server.persistence.PersistenceEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public interface ScopeTypeFunctionPersistence extends PersistenceEntity<ScopeTypeFunction> {

	static String computeScopeFunctionDerivableAsString(Boolean scopeFunctionDerivable) {
		return Boolean.TRUE.equals(scopeFunctionDerivable) ? "Oui" : "Non";
	}
	
}