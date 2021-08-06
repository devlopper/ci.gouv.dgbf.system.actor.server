package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfileRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActorProfileRequestBusinessImpl extends AbstractBusinessEntityImpl<ActorProfileRequest, ActorProfileRequestPersistence> implements ActorProfileRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
