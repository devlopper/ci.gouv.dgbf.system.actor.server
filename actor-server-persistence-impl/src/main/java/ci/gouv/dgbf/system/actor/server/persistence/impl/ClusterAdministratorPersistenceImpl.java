package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterAdministratorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterAdministrator;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ClusterAdministratorPersistenceImpl extends AbstractPersistenceEntityImpl<ClusterAdministrator> implements ClusterAdministratorPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}