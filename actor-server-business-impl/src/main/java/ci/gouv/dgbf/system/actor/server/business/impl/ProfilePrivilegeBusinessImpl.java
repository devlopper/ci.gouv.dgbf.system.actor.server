package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

@ApplicationScoped
public class ProfilePrivilegeBusinessImpl extends AbstractBusinessEntityImpl<ProfilePrivilege, ProfilePrivilegePersistence> implements ProfilePrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteCreateBefore__(ProfilePrivilege profilePrivilege, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(profilePrivilege, properties, function);
		if(StringHelper.isBlank(profilePrivilege.getIdentifier()) && profilePrivilege.getProfile() != null && profilePrivilege.getPrivilege() != null)
			profilePrivilege.setIdentifier(profilePrivilege.getProfile().getCode()+"_"+profilePrivilege.getPrivilege().getCode());
	}
	
}
