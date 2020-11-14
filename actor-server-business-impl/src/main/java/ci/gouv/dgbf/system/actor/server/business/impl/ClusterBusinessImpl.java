package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ClusterBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Cluster;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ClusterBusinessImpl extends AbstractBusinessEntityImpl<Cluster, ClusterPersistence> implements ClusterBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
