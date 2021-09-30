package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

public interface PrivilegeBusiness extends BusinessEntity<Privilege> {

	String REFRESH = "Privilege.refresh";
	@Transactional
	void refresh();
	
}