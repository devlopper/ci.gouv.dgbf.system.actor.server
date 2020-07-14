package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

@ApplicationScoped
public class IdentityBusinessImpl extends AbstractBusinessEntityImpl<Identity, IdentityPersistence> implements IdentityBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Identity createFromInterface(Identity.Interface identity) {
		if(identity == null)
			return null;
		Identity __identity__ = new Identity()
				.setIdentifier(identity.getElectronicMailAddress())
				.setFirstName(identity.getFirstName())
				.setLastNames(identity.getLastNames())
				.setElectronicMailAddress(identity.getElectronicMailAddress());
		create(__identity__);
		return __identity__;
	}

}
