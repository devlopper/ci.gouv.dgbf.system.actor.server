package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ClusterAdministratorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterAdministratorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterAdministrator;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ClusterAdministratorBusinessImpl extends AbstractBusinessEntityImpl<ClusterAdministrator, ClusterAdministratorPersistence> implements ClusterAdministratorBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
