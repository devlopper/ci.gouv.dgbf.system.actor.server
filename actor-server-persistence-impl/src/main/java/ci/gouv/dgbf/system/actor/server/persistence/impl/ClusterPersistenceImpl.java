package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Cluster;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ClusterPersistenceImpl extends AbstractPersistenceEntityImpl<Cluster> implements ClusterPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}