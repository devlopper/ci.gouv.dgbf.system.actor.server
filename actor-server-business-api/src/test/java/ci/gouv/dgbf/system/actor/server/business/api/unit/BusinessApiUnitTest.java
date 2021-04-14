package ci.gouv.dgbf.system.actor.server.business.api.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;

public class BusinessApiUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void requestBusiness_normalizeElectronicMailAddress(){
		assertThat(RequestBusiness.normalizeElectronicMailAddress("kycdev@gmail.com")).isEqualTo("kycdev@gmail.com");
		assertThat(RequestBusiness.normalizeElectronicMailAddress("kyc dev@gmail.com")).isEqualTo("kycdev@gmail.com");
		assertThat(RequestBusiness.normalizeElectronicMailAddress("KYC DEV @ GMAIL.COM")).isEqualTo("kycdev@gmail.com");
	}
}