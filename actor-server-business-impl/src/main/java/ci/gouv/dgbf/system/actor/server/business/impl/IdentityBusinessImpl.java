package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.string.StringHelper;
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
		if(StringHelper.isBlank(identity.getFirstName()))
			throw new RuntimeException("Le nom est obligatoire");
		if(StringHelper.isBlank(identity.getLastNames()))
			throw new RuntimeException("Le prénom est obligatoire");
		if(StringHelper.isBlank(identity.getElectronicMailAddress()))
			throw new RuntimeException("L'adresse de courriel est obligatoire");
		Identity __identity__ = new Identity()
				.setActOfAppointmentReference(identity.getActOfAppointmentReference())
				.setActOfAppointmentSignatory(identity.getActOfAppointmentSignatory())
				.setActOfAppointmentSignatureDate(identity.getActOfAppointmentSignatureDate())
				.setActOfAppointmentSignatureDateAsTimestamp(identity.getActOfAppointmentSignatureDateAsTimestamp())
				.setAdministrativeFunction(identity.getAdministrativeFunction())
				.setAdministrativeUnit(identity.getAdministrativeUnit())
				.setCivility(identity.getCivility())
				.setElectronicMailAddress(identity.getElectronicMailAddress())
				.setFirstName(identity.getFirstName())
				.setGroup(identity.getGroup())
				.setIdentifier(identity.getElectronicMailAddress())
				.setLastNames(identity.getLastNames())
				.setMobilePhoneNumber(identity.getMobilePhoneNumber())
				.setNames(identity.getNames())
				.setOfficePhoneExtension(identity.getOfficePhoneExtension())
				.setOfficePhoneNumber(identity.getOfficePhoneNumber())
				.setPostalBoxAddress(identity.getPostalBoxAddress())
				.setRegistrationNumber(identity.getRegistrationNumber())
				;
		if(__identity__.getActOfAppointmentSignatureDate() == null) {
			if(__identity__.getActOfAppointmentSignatureDateAsTimestamp() != null)
				__identity__.setActOfAppointmentSignatureDate(LocalDate.ofInstant(Instant.ofEpochMilli(__identity__.getActOfAppointmentSignatureDateAsTimestamp())
						,ZoneId.systemDefault()));
		}
		create(__identity__);
		return __identity__;
	}
}