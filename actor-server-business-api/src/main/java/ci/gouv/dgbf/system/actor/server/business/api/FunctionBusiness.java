package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

public interface FunctionBusiness extends BusinessEntity<Function> {

	@Transactional
	void importFormKeycloak();
	
	@Transactional
	void exportToKeycloak();
}
