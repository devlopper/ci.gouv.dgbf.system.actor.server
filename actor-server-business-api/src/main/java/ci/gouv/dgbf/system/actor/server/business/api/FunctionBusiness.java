package ci.gouv.dgbf.system.actor.server.business.api;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

public interface FunctionBusiness extends BusinessEntity<Function> {

	String SAVE = "Function.save";
}
