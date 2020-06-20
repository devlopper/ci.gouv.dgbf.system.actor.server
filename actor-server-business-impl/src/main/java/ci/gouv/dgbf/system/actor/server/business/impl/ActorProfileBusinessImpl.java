package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActorProfileBusinessImpl extends AbstractBusinessEntityImpl<ActorProfile, ActorProfilePersistence> implements ActorProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
