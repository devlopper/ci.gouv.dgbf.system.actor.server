package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentityGroupBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityGroupPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentityGroupBusinessImpl extends AbstractBusinessEntityImpl<IdentityGroup, IdentityGroupPersistence> implements IdentityGroupBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
